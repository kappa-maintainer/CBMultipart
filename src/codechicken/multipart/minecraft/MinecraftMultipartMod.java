package codechicken.multipart.minecraft;

import codechicken.lib.packet.PacketCustom;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

@Mod(modid = "MinecraftMultipart", acceptedMinecraftVersions="[1.10]")
public class MinecraftMultipartMod
{
    @Mod.Instance("MinecraftMultipart")
    public static MinecraftMultipartMod instance;

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event)
    {
        new Content().init();
        MinecraftForge.EVENT_BUS.register(new EventHandler());
        PacketCustom.assignHandler(McMultipartSPH.channel, new McMultipartSPH());
        if(FMLCommonHandler.instance().getSide().isClient())
            PacketCustom.assignHandler(McMultipartCPH.channel, new McMultipartCPH());

    }
}