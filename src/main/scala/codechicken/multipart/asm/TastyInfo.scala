package codechicken.multipart.asm

import net.minecraft.launchwrapper.Launch

import java.io.InputStream
import java.net.URLClassLoader
import java.nio.file.{Files, StandardCopyOption}
import java.util.concurrent.ConcurrentHashMap
import scala.quoted.*
import scala.tasty.inspector.{Inspector, Tasty, TastyInspector}

/** The small part of Scala 3 TASTy metadata required by the mixin compiler. */
final case class TastyInfo(name: String, isTrait: Boolean, parents: Seq[String]) {
    def traitParents: Seq[String] = parents.filterNot(TastyInfo.isConcreteClass)

    def classParent: Option[String] = parents.find(TastyInfo.isConcreteClass)
}

object TastyInfo {
    private val cache = new ConcurrentHashMap[String, TastyInfo]()

    def readIfPresent(className: String): Option[TastyInfo] = {
        val name = className.replace('/', '.')
        val resourceName = name.replace('.', '/') + ".tasty"
        val loaders = Seq(
            Thread.currentThread().getContextClassLoader,
            getClass.getClassLoader,
            ASMMixinCompiler.cl
        ).filter(_ != null).distinct
        if (loaders.exists(_.getResource(resourceName) != null)) Some(read(name)) else None
    }

    def read(className: String): TastyInfo = {
        val name = className.replace('/', '.')
        val cached = cache.get(name)
        if (cached != null) return cached

        val info = readUncached(name)
        val previous = cache.putIfAbsent(name, info)
        if (previous != null) previous else info
    }

    private def isConcreteClass(name: String): Boolean = {
        val internalName = name.replace('.', '/')
        val bytes = ASMMixinCompiler.getBytes(internalName)
        if (bytes != null) {
            val node = new org.objectweb.asm.tree.ClassNode()
            new org.objectweb.asm.ClassReader(bytes).accept(node, org.objectweb.asm.ClassReader.EXPAND_FRAMES)
            (node.access & org.objectweb.asm.Opcodes.ACC_INTERFACE) == 0
        } else {
            try {
                val clazz = ASMMixinCompiler.cl.loadClass(name)
                !clazz.isInterface
            } catch {
                case _: Throwable => false
            }
        }
    }

    private def readUncached(name: String): TastyInfo = {
        val resourceName = name.replace('.', '/') + ".tasty"
        val stream = resourceStream(resourceName).getOrElse {
            throw new IllegalArgumentException("Scala 3 TASTy resource not found for " + name + ": " + resourceName)
        }
        val temp = Files.createTempFile("fmp-tasty-", ".tasty")
        try {
            try Files.copy(stream, temp, StandardCopyOption.REPLACE_EXISTING)
            finally stream.close()
            val capture = new Capture(name)
            val success = TastyInspector.inspectAllTastyFiles(
                scala.collection.immutable.List(temp.toString),
                scala.collection.immutable.Nil,
                scala.collection.immutable.List.from(dependencyClasspath)
            )(capture)
            if (!success) {
                throw new IllegalArgumentException("Unable to read Scala 3 TASTy for " + name)
            }
            capture.info.getOrElse {
                throw new IllegalArgumentException("Scala 3 TASTy has no matching class definition for " + name)
            }
        } finally {
            Files.deleteIfExists(temp)
        }
    }

    private def dependencyClasspath: Seq[String] = {
        val loaders = Seq(
            Thread.currentThread().getContextClassLoader,
            getClass.getClassLoader,
            ASMMixinCompiler.cl
        ).filter(_ != null).distinct
        loaders.flatMap {
            case loader: URLClassLoader => loader.getURLs.toSeq.flatMap(url =>
                if (url.getProtocol == "file") Some(new java.io.File(url.toURI).getAbsolutePath) else None)
            case _ => Seq.empty
        }.distinct
    }

    private def resourceStream(resourceName: String): Option[InputStream] = {
        Option(Launch.classLoader.getResourceAsStream(resourceName))
    }

    private final class Capture(target: String) extends Inspector {
        var info: Option[TastyInfo] = None

        override def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
            import q.reflect.*

            object traverser extends TreeTraverser {
                override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
                    tree match {
                        case definition: ClassDef if definition.symbol.fullName == target =>
                            val symbol = definition.symbol
                            val parents = definition.parents.flatMap {
                                case parent: TypeTree =>
                                    val name = parent.tpe.typeSymbol.fullName
                                    if (name.nonEmpty) Some(name) else None
                                case _ => None
                            }
                            info = Some(TastyInfo(target, symbol.flags.is(Flags.Trait), parents))
                        case _ =>
                    }
                    super.traverseTree(tree)(owner)
                }
            }

            tastys.foreach(tasty => traverser.traverseTree(tasty.ast)(Symbol.spliceOwner))
        }
    }

}
