package codechicken.multipart.handler

import codechicken.lib.CodeChickenLib
import codechicken.multipart.{MultiPartRegistry, Reference, TickScheduler, WrappedTileEntityRegistry}
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.Mod.EventHandler
import net.minecraftforge.fml.common.event._

@Mod(modid = MultipartMod.modID, version = Reference.VERSION, dependencies = MultipartMod.deps, acceptedMinecraftVersions = CodeChickenLib.MC_VERSION_DEP, modLanguage = "scala")
object MultipartMod {
    final val modID = "forgemultipartcbe"
    final val deps = CodeChickenLib.MOD_VERSION_DEP + "required-after:forge@[14.23.5.2768,)"

    @EventHandler
    def preInit(event: FMLPreInitializationEvent): Unit = {
        MultipartProxy.preInit(event.getModConfigurationDirectory)
        WrappedTileEntityRegistry.init()
    }

    @EventHandler
    def init(event: FMLInitializationEvent): Unit = {
        MultipartProxy.init()
    }

    @EventHandler
    def postInit(event: FMLPostInitializationEvent): Unit = {
        if (MultiPartRegistry.required) {
            MultiPartRegistry.postInit()
            MultipartProxy.postInit()
        }
    }

    @EventHandler
    def beforeServerStart(event: FMLServerAboutToStartEvent): Unit = {
        TickScheduler.onServerStarting(event.getServer)
        MultiPartRegistry.beforeServerStart()
    }
}
