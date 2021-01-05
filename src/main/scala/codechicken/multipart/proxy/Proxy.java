package codechicken.multipart.proxy;

import codechicken.lib.world.TileChunkLoadHook;
import codechicken.multipart.capability.CapabilityMerger;
import codechicken.multipart.capability.MergedItemHandler;
import codechicken.multipart.handler.PlacementConversionHandler;
import codechicken.multipart.network.MultiPartNetwork;
import codechicken.multipart.network.MultiPartSPH;
import codechicken.multipart.util.MultiPartGenerator;
import codechicken.multipart.util.MultiPartLoadHandler;
import codechicken.multipart.util.TickScheduler;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLDedicatedServerSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLLoadCompleteEvent;
import net.minecraftforge.items.CapabilityItemHandler;

/**
 * Created by covers1624 on 30/8/20.
 */
public class Proxy {

    public void commonSetup(FMLCommonSetupEvent event) {
        MultiPartGenerator.INSTANCE.loadAnnotations();
        MultiPartLoadHandler.init();
        MultiPartNetwork.init();
        MultiPartSPH.init();
        PlacementConversionHandler.init();
        //        MinecraftForge.EVENT_BUS.register(ItemPlacementHelper$.MODULE$);
        TickScheduler.init();
        TileChunkLoadHook.init();
    }

    public void clientSetup(FMLClientSetupEvent event) {
    }

    public void serverSetup(FMLDedicatedServerSetupEvent event) {
    }

    public void onLoadComplete(FMLLoadCompleteEvent event) {
        CapabilityMerger.addMerger(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, MergedItemHandler::merge);
    }
}
