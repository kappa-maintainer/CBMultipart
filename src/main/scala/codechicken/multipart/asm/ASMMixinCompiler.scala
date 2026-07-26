package codechicken.multipart.asm

import java.io.{File, FileOutputStream}
import java.lang.reflect.{Method, Modifier}
import java.util.Set as JSet
import codechicken.asm.ASMHelper.*
import codechicken.asm.{ASMHelper, InsnComparator, InsnListSection, ModularASMTransformer}
import codechicken.lib.reflect.ObfMapping
import codechicken.lib.util.ResourceUtils
import codechicken.multipart.asm.ASMImplicits.*
import codechicken.multipart.asm.DebugPrinter.logger
import codechicken.multipart.handler.MultipartProxy
import net.minecraft.launchwrapper.Launch
import net.minecraft.launchwrapper.LaunchClassLoader
import net.minecraftforge.fml.common.asm.transformers.deobf.FMLDeobfuscatingRemapper
import net.minecraftforge.fml.relauncher.FMLLaunchHandler
import org.apache.logging.log4j.{LogManager, Logger}
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.Type.*
import org.objectweb.asm.tree.*
import org.objectweb.asm.{ClassReader, MethodVisitor, Type}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.collection.mutable.{ListBuffer as MList, Map as MMap, Set as MSet}
import scala.collection.View
import scala.language.implicitConversions

object DebugPrinter {
    if(MultipartProxy.config != null) MultipartProxy.config.removeTag("debug_asm")
    val debug: Boolean = ModularASMTransformer.DEBUG
    val logger: Logger = LogManager.getLogger("Multipart ASM")

    private var permGenUsed = 0
    val dir = new File("asm/multipart")
    if (debug) {
        if (!dir.exists) {
            dir.mkdirs()
        }
        for (file <- dir.listFiles)
            file.delete
    }

    def dump(name: String, bytes: Array[Byte]): Unit = {
        val fName = name.replace('/', '#')
        if(ModularASMTransformer.DUMP_RAW) {
            val file = ResourceUtils.ensureExists(new File(dir, fName + ".class"))
            val fos = new FileOutputStream(file)
            fos.write(bytes)
            fos.flush()
            fos.close()
        } else if (ModularASMTransformer.DUMP_TEXT) {
            ASMHelper.dump(bytes, new File(dir, fName + ".txt"), false, false, false)
        }
    }

    def defined(name: String, bytes: Array[Byte]): Unit = {
        if ((permGenUsed + bytes.length) / 16000 != permGenUsed / 16000) {
            logger.debug(s"${permGenUsed + bytes.length} bytes of permGen has been used by ASMMixinCompiler")
        }

        permGenUsed += bytes.length
    }
}

object ASMMixinCompiler {
    val cl: LaunchClassLoader = Launch.classLoader

    private val traitByteMap = mutable.Map[String, Array[Byte]]()
    private val mixinMap = mutable.Map[String, MixinInfo]()

    def define(name: String, bytes: Array[Byte]) = {
        internalDefine(name, bytes)
        DebugPrinter.defined(name, bytes)

        try {
            Launch.classLoader.defineClass(name.replace('/', '.'), bytes)
        } catch {
            case link: LinkageError if link.getMessage.contains("duplicate") =>
                throw new IllegalStateException("class with name: " + name + " already loaded. Do not reference your java mixin classes before registering", link)
        }
    }

    getBytes("net/minecraftforge/fml/common/asm/FMLSanityChecker")

    def getBytes(name: String): Array[Byte] = {
        val jName = name.replace('/', '.')
        if (jName == "java.lang.Object") {
            return null
        }

        def useTransformers = Launch.classLoader.getInvalidClasses.asScala.exists(jName.startsWith)
        val obfName = if (ObfMapping.obfuscated) FMLDeobfuscatingRemapper.INSTANCE.unmap(name).replace('/', '.') else jName
        val bytes = cl.getClassBytes(obfName)
        if (bytes != null && useTransformers) {
            return Launch.classLoader.runTransformers(obfName, jName, bytes)
        }

        bytes
    }

    def internalDefine(name$: String, bytes: Array[Byte]): Unit = {
        val name = nodeName(name$)
        traitByteMap.put(name, bytes)
        remClassInfo(name)
        DebugPrinter.dump(name, bytes)
    }

    def classNode(name$: String): ClassNode = {
        val name = nodeName(name$)
        traitByteMap.getOrElseUpdate(name, getBytes(name)) match {
            case null => null
            case v => createClassNode(v, ClassReader.EXPAND_FRAMES)
        }
    }

    def getMixinInfo(name: String) = mixinMap.get(name)

    case class FieldMixin(name: String, desc: String, access: Int) {
        def accessName(owner: String): String = if ((access & ACC_PRIVATE) != 0) {
            owner.replace('/', '$') + "$$" + name
        } else {
            name
        }
    }

    case class SuperBridge(name: String, targetName: String, desc: String)

    case class MixinInfo(name: String, parent: String, parentTraits: Seq[MixinInfo],
                         fields: Seq[FieldMixin], methods: Seq[MethodNode], supers: Seq[SuperBridge],
                         implementationOwner: String = null, implementationSuffix: String = "",
                         hasInitializer: Boolean = true, implementationIsInterface: Boolean = false) {
        def linearise: Seq[MixinInfo] = parentTraits.flatMap(_.linearise) :+ this

        def implementationClass: String = if (implementationOwner == null) name + "$class" else implementationOwner

        def implementationName(method: String): String = if (method == "$init$") method else method + implementationSuffix
    }

    abstract class MethodInfo {
        def owner: ClassInfo

        def name: String

        def desc: String

        def exceptions: Array[String]

        def isPrivate: Boolean

        def isAbstract: Boolean

        override def toString: String = owner.name + "." + name + desc
    }

    abstract class ClassInfo {
        def name: String

        def superClass: Option[ClassInfo]

        def interfaces: Iterable[ClassInfo]

        def methods: Iterable[MethodInfo]

        override def toString: String = getClass.getName.replaceAll(".+[$.]", "") + "(" + name + ")"

        def parentMethods: View[MethodInfo] = (superClass ++ interfaces).view.flatMap(_.allMethods)

        def allMethods: Iterable[MethodInfo] = methods ++ parentMethods

        def findPublicImpl(name: String, desc: String): Option[MethodInfo] = allMethods.find(m => m.name == name && m.desc == desc && !m.isAbstract && !m.isPrivate)

        def isScala = false

        def isTrait = false

        def tastyInfo: Option[TastyInfo] = None

        def isObject = false

        def moduleName: String = name
    }

    private val infoCache = mutable.Map[String, ClassInfo]()

    def remClassInfo(name: String): Option[ClassInfo] = infoCache.remove(name)

    implicit def getClassInfo(name: String): ClassInfo = infoCache.getOrElseUpdate(name, ClassInfo.obtainInfo(name))

    implicit def getClassInfo(cnode: ClassNode): ClassInfo = getClassInfo(cnode.name)

    implicit def getClassInfo(clazz: Class[?]): ClassInfo = if (clazz == null) null else getClassInfo(clazz.nodeName)

    object ClassInfo {

        class ReflectionClassInfo(clazz: Class[?]) extends ClassInfo {

            case class ReflectionMethodInfo(method: Method) extends MethodInfo {
                def owner: ReflectionClassInfo = ReflectionClassInfo.this

                def name: String = method.getName

                def desc: String = getType(method).getDescriptor

                def exceptions: Array[String] = method.getExceptionTypes.map(_.nodeName)

                def isPrivate: Boolean = Modifier.isPrivate(method.getModifiers)

                def isAbstract: Boolean = Modifier.isAbstract(method.getModifiers)
            }

            def name: String = clazz.nodeName

            def superClass = Option(clazz.getSuperclass)

            def interfaces: mutable.Iterable[ClassInfo] = clazz.getInterfaces.map(getClassInfo)

            def methods: mutable.Iterable[ReflectionMethodInfo] = clazz.getMethods.map(this.ReflectionMethodInfo.apply)
        }

        class ClassNodeInfo(val cnode: ClassNode) extends ClassInfo {

            case class MethodNodeInfoSource(mnode: MethodNode) extends MethodInfo {
                def owner: ClassNodeInfo = ClassNodeInfo.this

                def name: String = mnode.name

                def desc: String = mnode.desc

                def exceptions: Array[String] = Array(mnode.exceptions.asScala.toSeq *)

                def isPrivate: Boolean = (mnode.access & ACC_PRIVATE) != 0

                def isAbstract: Boolean = (mnode.access & ACC_ABSTRACT) != 0
            }

            def name: String = cnode.name

            def superClass: Option[ClassInfo] = Some(cnode.superName)

            def interfaces: Seq[ClassInfo] = cnode.interfaces.asScala.map(getClassInfo).toSeq

            def methods: mutable.Buffer[MethodNodeInfoSource] = cnode.methods.asScala.map(this.MethodNodeInfoSource.apply)
        }

        class TastyClassInfo(cnode$: ClassNode, val tasty: TastyInfo) extends ClassNodeInfo(cnode$) {
            override def superClass: Option[ClassInfo] = tasty.classParent.map(getClassInfo)

            override def interfaces: Seq[ClassInfo] = cnode$.interfaces.asScala.map(getClassInfo).toSeq

            override def isScala = true

            override def isTrait: Boolean = tasty.isTrait

            override def tastyInfo = Some(tasty)
        }

        private[ASMMixinCompiler] def obtainInfo(name: String): ClassInfo = {
            if (name == null) return null

            classNode(name) match {
                case null => cl.findClass(name.replace('/', '.')) match {
                    case null => null
                    case c => new ReflectionClassInfo(c)
                }
                case cnode => TastyInfo.readIfPresent(cnode.name.replace('/', '.'))
                    .map(new TastyClassInfo(cnode, _))
                    .getOrElse(new ClassNodeInfo(cnode))
            }
        }
    }

    import StackAnalyser.width

    def finishBridgeCall(mv: MethodVisitor, mvdesc: String, opcode: Int, owner: String, name: String, desc: String,
                         isInterface: Boolean): Unit = {
        val args = getArgumentTypes(mvdesc)
        val ret = getReturnType(mvdesc)
        var localIndex = 1
        args.foreach {
            arg =>
                mv.visitVarInsn(arg.getOpcode(ILOAD), localIndex)
                localIndex += width(arg)
        }
        // The constant-pool reference kind is independent of the opcode: Scala trait
        // helpers use INVOKESTATIC with an interface owner.
        mv.visitMethodInsn(opcode, owner, name, desc, isInterface)
        mv.visitInsn(ret.getOpcode(IRETURN))
        mv.visitMaxs(Math.max(width(args) + 1, width(ret)), width(args) + 1)
    }

    def writeBridge(mv: MethodVisitor, mvdesc: String, opcode: Int, owner: String, name: String, desc: String,
                    isInterface: Boolean): Unit = {
        mv.visitVarInsn(ALOAD, 0)
        finishBridgeCall(mv, mvdesc, opcode, owner, name, desc, isInterface)
    }

    def writeStaticBridge(mv: MethodNode, mname: String, t: MixinInfo) =
        writeBridge(mv, mv.desc, INVOKESTATIC, t.implementationClass,
            t.implementationName(mname), staticDesc(t.name, mv.desc), t.implementationIsInterface)

    def mixinClasses(name: String, superClass: String, traits: Seq[String]): Class[?] = {
        if (traits.isEmpty) {
            return cl.findClass(superClass.name.replace('/', '.'))
        }

        val startTime = System.currentTimeMillis

        val baseTraits = traits.map(mixinMap)
        val mixinInfos = baseTraits.flatMap(_.linearise).distinct
        val baseInfo = getClassInfo(superClass)
        val traitInfos = mixinInfos.map(i => getClassInfo(i.name))

        val cnode = new ClassNode()
        //implements list
        cnode.visit(V25, ACC_PUBLIC, name, null, superClass, baseTraits.map(_.name).toArray[String])

        val cinit = baseInfo.methods.find(_.name == "<init>").get
        val minit = cnode.visitMethod(ACC_PUBLIC, "<init>", cinit.desc, null, null).asInstanceOf[MethodNode]
        writeBridge(minit, cinit.desc, INVOKESPECIAL, superClass, "<init>", cinit.desc, false)
        minit.instructions.remove(minit.instructions.getLast) //remove the RETURN from writeBridge

        val prevInfos = MList[MixinInfo]()

        mixinInfos.foreach { t =>
            if (t.hasInitializer) {
                minit.visitVarInsn(ALOAD, 0)
                minit.visitMethodInsn(INVOKESTATIC, t.implementationClass,
                    t.implementationName("$init$"), "(L" + t.name + ";)V", t.implementationIsInterface)
            }

            t.fields.foreach { f =>
                val fv = cnode.visitField(ACC_PRIVATE, f.accessName(t.name), f.desc, null, null).asInstanceOf[FieldNode]

                val ftype = getType(fv.desc)
                var mv = cnode.visitMethod(ACC_PUBLIC, fv.name, "()" + f.desc, null, null)
                mv.visitVarInsn(ALOAD, 0)
                mv.visitFieldInsn(GETFIELD, name, fv.name, fv.desc)
                mv.visitInsn(ftype.getOpcode(IRETURN))
                mv.visitMaxs(1, 1)

                mv = cnode.visitMethod(ACC_PUBLIC, fv.name + "_$eq", "(" + f.desc + ")V", null, null)
                mv.visitVarInsn(ALOAD, 0)
                mv.visitVarInsn(ftype.getOpcode(ILOAD), 1)
                mv.visitFieldInsn(PUTFIELD, name, fv.name, fv.desc)
                mv.visitInsn(RETURN)
                mv.visitMaxs(width(ftype) + 1, width(ftype) + 1)
            }

            t.supers.foreach { bridge =>
                val mv = cnode.visitMethod(ACC_PUBLIC, bridge.name, bridge.desc, null, null).asInstanceOf[MethodNode]

                prevInfos.findLast(_.methods.exists(m => m.name == bridge.targetName && m.desc == bridge.desc)) match {
                    // Each super call goes to the previous trait implementation.
                    case Some(st) => writeStaticBridge(mv, bridge.targetName, st)
                    case None =>
                        writeBridge(mv, bridge.desc, INVOKESPECIAL,
                            baseInfo.findPublicImpl(bridge.targetName, bridge.desc).get.owner.name,
                            bridge.targetName, bridge.desc, false)
                }
            }

            prevInfos += t
        }

        val methodSigs = mutable.Set[String]()
        mixinInfos.reverse.foreach { t => //last trait gets first pick on methods
            t.methods.foreach { m =>
                if (!methodSigs(m.name + m.desc)) {
                    val mv = cnode.visitMethod(ACC_PUBLIC, m.name, m.desc, null, Array(m.exceptions.asScala.toSeq*)).asInstanceOf[MethodNode]
                    writeStaticBridge(mv, m.name, t)
                    methodSigs += m.name + m.desc
                }
            }
        }

        minit.visitInsn(RETURN)

        //generate synthetic bridge methods for covariant return types
        def allParents(info: ClassInfo): Iterable[ClassInfo] = info +: (info.superClass ++ info.interfaces).toSeq.flatMap(allParents)

        val allParentInfos = (baseInfo +: traitInfos).flatMap(allParents).distinct
        val allParentMethods = allParentInfos.flatMap(_.methods)
        methodSigs.toSeq.foreach { nameDesc =>
            val (name, desc) = seperateDesc(nameDesc)
            val pDesc = desc.substring(0, desc.lastIndexOf(')') + 1)

            allParentMethods.filter(m => m.name == name && m.desc.startsWith(pDesc)).foreach { m =>
                if (!methodSigs(m.name + m.desc)) {
                    val mv = cnode.visitMethod(ACC_PUBLIC | ACC_SYNTHETIC | ACC_BRIDGE, m.name, m.desc, null, m.exceptions).asInstanceOf[MethodNode]
                    writeBridge(mv, mv.desc, INVOKEVIRTUAL, cnode.name, name, desc, false)
                    methodSigs += m.name + m.desc
                }
            }
        }

        val c = define(name, createBytes(cnode, 0))

        DebugPrinter.logger.debug("Generation [" + superClass + " with " + traits.mkString(", ") + "] took " + (System.currentTimeMillis - startTime) + "ms")
        c
    }

    def seperateDesc(nameDesc: String): (String, String) = {
        val n = nameDesc.indexOf('(')
        (nameDesc.substring(0, n), nameDesc.substring(n))
    }

    def staticDesc(owner: String, desc: String): String = {
        val descT = getMethodType(desc)
        getMethodDescriptor(descT.getReturnType, getType("L" + owner + ";") +: descT.getArgumentTypes *)
    }

    def getSuper(minsn: MethodInsnNode, stack: StackAnalyser): Option[MethodInfo] = {
        import StackAnalyser._

        if (minsn.owner == stack.owner.getInternalName) {
            return None
        } //not a super call

        //super calls are either to methods with the same name or contain a pattern 'target$$super$name' from the scala compiler
        val methodName = stack.m.name.replaceAll(".+\\Q$$super$\\E", "")
        if (minsn.name != methodName) {
            return None
        }

        stack.peek(Type.getType(minsn.desc).getArgumentTypes.length) match {
            case Load(This(o)) =>
            case _ => return None //have to be invoked on this
        }

        getClassInfo(stack.owner.getInternalName).superClass.flatMap(_.findPublicImpl(methodName, minsn.desc))
    }

    def getAndRegisterParentTraits(cnode: ClassNode): Seq[MixinInfo] = {
        val info = getClassInfo(cnode)
        val names = info.tastyInfo.map(_.traitParents).getOrElse(cnode.interfaces.asScala.map(_.replace('/', '.')).toSeq)
        names.flatMap { name =>
            val parent = getClassInfo(name)
            if (parent != null && parent.isTrait) Some(registerScalaTrait(classNode(parent.name))) else None
        }
    }

    def registerJavaTrait(cnode: ClassNode): Unit = {
        if ((cnode.access & ACC_INTERFACE) != 0) {
            throw new IllegalArgumentException("Cannot register java interface " + cnode.name + " as a mixin trait. Try register passThroughInterface")
        }
        if (!cnode.innerClasses.isEmpty) {
            throw new IllegalArgumentException("Inner classes are not permitted for " + cnode.name + " as a java mixin trait. Use scala")
        }
        if ((cnode.access & ACC_ABSTRACT) != 0) {
            throw new IllegalArgumentException("Cannot register abstract class " + cnode.name + " as a java mixin trait. Use scala")
        }


        //val parentTraits = getAndRegisterParentTraits(cnode)
        val fields = cnode.fields.asScala.map(f => (f.name, FieldMixin(f.name, f.desc, f.access))).toMap
        val supers = MList[SuperBridge]()

        def superBridge(name: String, desc: String): SuperBridge = {
            val targetName = name.substring(name.lastIndexOf("$$super$") + "$$super$".length)
            SuperBridge(name, targetName, desc)
        }
        val methods = MList[MethodNode]()
        val methodSigs = cnode.methods.asScala.map(m => m.name + m.desc).toSet

        /*if ((cnode.access & ACC_ABSTRACT) != 0) {//verify all methods are implemented
            def getInterfaces(cnode:ClassNode):Seq[ClassNode] = cnode.interfaces.map(classNode).flatMap(i => getInterfaces(i) :+ i)
            val interfaces = getInterfaces(cnode).distinct
            val implementedSigs = (cnode.methods.filter(m => (m.access & ACC_ABSTRACT) == 0)++parentTraits.flatMap(_.methods)).map(m => m.name + m.desc).toSet
            val missing = interfaces.flatMap(_.methods).map(m => m.name + m.desc).filterNot(implementedSigs)
            if(!missing.isEmpty)
                throw new IllegalArgumentException("Abstract java trait "+cnode.name+" needs to implement "+missing.mkString(", "))
        }*/

        val inode = new ClassNode() //impl node
        inode.visit(V25, ACC_ABSTRACT | ACC_PUBLIC, cnode.name + "$class", null, "java/lang/Object", null)
        inode.sourceFile = cnode.sourceFile

        val tnode = new ClassNode() //trait node (interface)
        tnode.visit(V25, ACC_INTERFACE | ACC_ABSTRACT | ACC_PUBLIC, cnode.name, null, "java/lang/Object", Array(cnode.interfaces.asScala.toSeq*))

        def fname(name: String) = fields(name).accessName(cnode.name)

        fields.values.foreach { fnode =>
            tnode.visitMethod(ACC_PUBLIC | ACC_ABSTRACT, fname(fnode.name), "()" + fnode.desc, null, null)
            tnode.visitMethod(ACC_PUBLIC | ACC_ABSTRACT, fname(fnode.name) + "_$eq", "(" + fnode.desc + ")V", null, null)
        }

        def superInsn(minsn: MethodInsnNode) = {
            val generated = SuperBridge(cnode.name.replace('/', '$') + "$$super$" + minsn.name, minsn.name, minsn.desc)
            val bridge = supers.find(b => b.targetName == generated.targetName && b.desc == generated.desc).getOrElse {
                tnode.visitMethod(ACC_PUBLIC | ACC_ABSTRACT, generated.name, generated.desc, null, null)
                supers += generated
                generated
            }
            new MethodInsnNode(INVOKEINTERFACE, cnode.name, bridge.name, bridge.desc, true)
        }

        def staticClone(mnode: MethodNode, name: String, access: Int) = {
            val mv = inode.visitMethod(access | ACC_STATIC, name,
                staticDesc(cnode.name, mnode.desc),
                null, Array(mnode.exceptions.asScala.toSeq*)).asInstanceOf[MethodNode]
            copy(mnode, mv)
            mv
        }

        def staticTransform(mnode: MethodNode, base: MethodNode): Unit = {
            val stack = new StackAnalyser(getObjectType(cnode.name), base)
            val insnList = mnode.instructions
            var insn = insnList.getFirst
            // Scala 3 emits trait-super bridges on concrete helper classes. Once the
            // method is moved to the synthetic $class, its original invokespecial is
            // no longer valid because that helper does not inherit from the target.
            val isScalaSuperBridge = base.name.contains("$$super$")

            def replace(newinsn: AbstractInsnNode): Unit = {
                insnList.insert(insn, newinsn)
                insnList.remove(insn)
                insn = newinsn
            }

            //transform
            while (insn != null) {
                insn match {
                    case finsn: FieldInsnNode => insn.getOpcode match {
                        case GETFIELD => replace(new MethodInsnNode(INVOKEINTERFACE, cnode.name,
                            fname(finsn.name), "()" + finsn.desc, true))
                        case PUTFIELD => replace(new MethodInsnNode(INVOKEINTERFACE, cnode.name,
                            fname(finsn.name) + "_$eq", "(" + finsn.desc + ")V", true))
                        case _ =>
                    }
                    case minsn: MethodInsnNode => insn.getOpcode match {
                        case INVOKESPECIAL =>
                            if (isScalaSuperBridge || getSuper(minsn, stack).isDefined) {
                                replace(superInsn(minsn))
                            }
                        case INVOKEVIRTUAL =>
                            if (minsn.owner == cnode.name) {
                                if (methodSigs.contains(minsn.name + minsn.desc)) {
                                    //call the interface method
                                    replace(new MethodInsnNode(INVOKEINTERFACE, minsn.owner, minsn.name, minsn.desc, true))
                                } else {
                                    //cast to parent class and call
                                    val mType = Type.getMethodType(minsn.desc)
                                    val instanceEntry = stack.peek(width(mType.getArgumentTypes))
                                    insnList.insert(instanceEntry.insn, new TypeInsnNode(CHECKCAST, cnode.superName))
                                    minsn.owner = cnode.superName
                                }
                            }
                        case _ =>
                    }
                    case _ =>
                }
                stack.visitInsn(insn)
                insn = insn.getNext
            }
        }

        def convertMethod(mnode: MethodNode): Unit = {
            if (mnode.name == "<clinit>") {
                throw new IllegalArgumentException("Static initialisers are not permitted " + cnode.name + " as a mixin trait")
            }

            if (mnode.name == "<init>") {
                if (mnode.desc != "()V") {
                    throw new IllegalArgumentException("Constructor arguments are not permitted " + cnode.name + " as a mixin trait")
                }

                val mv = staticClone(mnode, "$init$", ACC_PUBLIC)

                def removeSuperConstructor(): Unit = {
                    val insns = new InsnListSection
                    insns.add(new VarInsnNode(ALOAD, 0))
                    insns.add(new MethodInsnNode(INVOKESPECIAL, cnode.superName, "<init>", "()V", false))

                    val minsns = new InsnListSection(mv.instructions)
                    val found = InsnComparator.matches(minsns, insns, Set[LabelNode]().asJava)
                    if (found == null) {
                        throw new IllegalArgumentException("Invalid constructor insn sequence " + cnode.name + "\n" + minsns)
                    }
                    found.trim(Set[LabelNode]().asJava).remove()
                }

                removeSuperConstructor()
                staticTransform(mv, mnode)
                return
            }

            if ((mnode.access & ACC_PRIVATE) == 0) {
                val mv = tnode.visitMethod(ACC_PUBLIC | ACC_ABSTRACT, mnode.name, mnode.desc, null, Array(mnode.exceptions.asScala.toSeq*))
                methods += mv.asInstanceOf[MethodNode]
            }

            //convert that method!
            val access = if ((mnode.access & ACC_PRIVATE) == 0) ACC_PUBLIC else ACC_PRIVATE
            val mv = staticClone(mnode, mnode.name, access)
            staticTransform(mv, mnode)
        }

        cnode.methods.asScala.filter(_.name.contains("$$super$")).foreach { method =>
            val bridge = superBridge(method.name, method.desc)
            if (!supers.exists(_.name == bridge.name)) {
                tnode.visitMethod(ACC_PUBLIC | ACC_ABSTRACT, bridge.name, bridge.desc, null, null)
                supers += bridge
            }
        }
        cnode.methods.asScala.filterNot(_.name.contains("$$super$")).foreach(convertMethod)

        define(inode.name, createBytes(inode, 0))
        define(tnode.name, createBytes(tnode, 0))

        mixinMap.put(tnode.name, MixinInfo(tnode.name, cnode.superName, Seq(),
            fields.values.toSeq, methods.toSeq, supers.toSeq))
    }

    private def sideOnly(annotations: java.util.List[AnnotationNode]): Boolean = {
        if (annotations == null) return false
        val side = FMLLaunchHandler.side.name
        annotations.asScala.exists { annotation =>
            annotation.desc == "Lnet/minecraftforge/fml/relauncher/SideOnly;" &&
                annotation.values != null && annotation.values.asScala.grouped(2).exists {
                    case Seq("value", value: Array[?]) => value.lastOption.exists(_.toString != side)
                    case _ => false
                }
        }
    }

    private def sideOnly(method: MethodNode): Boolean =
        sideOnly(method.visibleAnnotations) || sideOnly(method.invisibleAnnotations)

    def registerScalaTrait(cnode: ClassNode): MixinInfo = {
        getMixinInfo(cnode.name) match {
            case Some(info) => return info
            case None =>
        }

        val info = getClassInfo(cnode)
        if (!info.isTrait) {
            throw new IllegalArgumentException(cnode.name + " is not a Scala 3 trait")
        }

        val parentTraits = getAndRegisterParentTraits(cnode)
        val abstractMethods = cnode.methods.asScala.filter { method =>
            (method.access & ACC_ABSTRACT) != 0 &&
                (method.access & ACC_STATIC) == 0 &&
                !sideOnly(method)
        }
        val getters = abstractMethods.filter { method =>
            !method.name.contains("$$super$") &&
                !method.name.endsWith("_$eq") &&
                method.desc.startsWith("()")
        }.map(method => method.name -> method).toMap
        val setters = abstractMethods.filter(_.name.endsWith("_$eq"))
            .map(method => method.name.stripSuffix("_$eq") -> method).toMap
        val fields = getters.collect {
            case (name, getter) if setters.contains(name) =>
                val fieldName = if (name.contains("$$")) name.substring(name.lastIndexOf("$$") + 2) else name
                FieldMixin(fieldName, getReturnType(getter.desc).getDescriptor,
                    if (name.contains("$$")) ACC_PRIVATE else ACC_PUBLIC)
        }.toSeq
        val supers = abstractMethods.filter(_.name.contains("$$super$")).map { method =>
            val marker = "$$super$"
            SuperBridge(method.name, method.name.substring(method.name.indexOf(marker) + marker.length), method.desc)
        }.toSeq
        val methods = cnode.methods.asScala.filter { method =>
            (method.access & (ACC_ABSTRACT | ACC_STATIC | ACC_PRIVATE)) == 0 &&
                method.name != "<init>" && method.name != "$init$" &&
                !sideOnly(method)
        }.toSeq
        val hasInitializer = cnode.methods.asScala.exists { method =>
            method.name == "$init$" && (method.access & ACC_STATIC) != 0
        }
        val parent = info.superClass.map(_.name)
            .orElse(info.tastyInfo.flatMap(_.classParent))
            .orElse(parentTraits.headOption.map(_.parent))
            .getOrElse("java/lang/Object")
        val mixin = MixinInfo(cnode.name, parent, parentTraits.toSeq, fields, methods, supers,
            cnode.name, "$", hasInitializer, (cnode.access & ACC_INTERFACE) != 0)
        mixinMap.put(cnode.name, mixin)
        mixin
    }
}
