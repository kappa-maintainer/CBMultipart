package codechicken.multipart.asm

import java.lang.reflect.Constructor
import java.util.{BitSet => JBitSet}

import codechicken.multipart.asm.ASMImplicits._
import codechicken.multipart.asm.ASMMixinCompiler._
import org.objectweb.asm.tree.ClassNode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ASMMixinFactory[T](val baseType: Class[T], private val paramTypes: Class[?]*) {
    private val traitMap = mutable.Map[String, Int]()
    private val traits = ArrayBuffer[String]()
    private val classMap = mutable.Map[JBitSet, Constructor[? <: T]]()

    private var ugenid = 0

    private def nextName() = {
        val ret = baseType.getSimpleName + "_cmp$$" + ugenid
        ugenid += 1
        ret
    }

    private def compile(traitSet: JBitSet) = {
        val seq = Seq.newBuilder[String]
        var i = -1
        while ( {
            i = traitSet.nextSetBit(i + 1); i >= 0
        })
            seq += traits(i)

        val c = ASMMixinCompiler.mixinClasses(nextName(), baseType.nodeName, seq.result()).asInstanceOf[Class[? <: T]]
        onCompiled(c, traitSet)
        c.getDeclaredConstructor(paramTypes*)
    }

    protected def onCompiled(clazz: Class[? <: T], traitSet: JBitSet): Unit = {}

    protected def autoCompleteJavaTrait(cnode: ClassNode): Unit = {}

    def construct(traitSet: JBitSet, args: Object*) = synchronized {
        (classMap.get(traitSet) match {
            case Some(c) => c
            case None =>
                val c = compile(traitSet)
                classMap.put(traitSet.copy, c)
                c
        }).newInstance(args*)
    }

    def getId(s_trait: String) = traitMap(s_trait)

    def registerTrait(traitClass: Class[?]): Int = registerTrait(traitClass.nodeName)

    def registerTrait(s_trait: String): Int = {
        val cnode = classNode(s_trait)
        if (cnode == null) {
            throw new ClassNotFoundException(s_trait)
        }

        traitMap.get(cnode.name) match {
            case Some(id) => return id
            case None =>
        }

        val info = getClassInfo(cnode)

        def concreteParent(info: ClassInfo): ClassInfo = info.superClass.map {
            case i if i.isTrait => concreteParent(i)
            case i => i
        }.get

        val parentName = concreteParent(info).name

        def checkParent(info: ClassInfo): Boolean = info.name == parentName || info.superClass.exists(checkParent)

        if (!checkParent(getClassInfo(baseType))) {
            throw new IllegalArgumentException(baseType.nodeName + " does not extend parent " + parentName + " of mixin trait " + s_trait)
        }

        if (info.isTrait) {
            registerScalaTrait(cnode)
        }
        else {
            autoCompleteJavaTrait(cnode)
            registerJavaTrait(cnode)
        }

        val id = traits.size
        traits += cnode.name
        traitMap.put(cnode.name, id)
        id
    }
}
