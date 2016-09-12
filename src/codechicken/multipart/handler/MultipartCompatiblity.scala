package codechicken.multipart.handler

import net.minecraft.block.Block
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.fml.common.FMLCommonHandler

object MultipartCompatiblity
{
    var canAddPart = (world:World, pos:BlockPos) => true

    def load() {
        if(FMLCommonHandler.instance().getModName.contains("mcpc"))
            MCPCCompatModule.load()
    }
}

object MCPCCompatModule
{
    def load() {
        try {
//            val m_canPlacePart = classOf[World].getDeclaredMethod("canPlaceMultipart", classOf[Block], classOf[Int], classOf[Int], classOf[Int])
//            MultipartCompatiblity.canAddPart = (world:World, x:Int, y:Int, z:Int) => {
//                m_canPlacePart.invoke(world, MultipartProxy.block, x:Integer, y:Integer, z:Integer).asInstanceOf[Boolean].booleanValue()
//            }
        } catch {
            case e:Exception => MultipartProxy.logger.error("Failed to integrate MCPC placement hooks", e)
        }
    }
}


