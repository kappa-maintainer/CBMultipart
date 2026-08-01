package codechicken.multipart

import codechicken.multipart.handler.MultipartSPH
import net.minecraft.entity.EntityLivingBase
import net.minecraft.util.math.{AxisAlignedBB, BlockPos}
import net.minecraft.world.WorldServer

import scala.jdk.CollectionConverters._

/**
 * Server-safe multipart particle selection. A BlockMultipart state is shared
 * by every part and therefore cannot identify the sprite an entity contacted.
 */
object MultipartParticleEffects {

    private final val SupportTolerance = 0.125D
    private final val IntersectionEpsilon = 1.0E-5D

    /**
     * Finds the highest collision box at an entity's feet. Collision boxes are
     * used rather than display bounds because a part may have several physical
     * extensions (for example, framed-wire connections).
     */
    def findSupportPart(tile: TileMultipart, pos: BlockPos, entityBox: AxisAlignedBB): Int = {
        val footY = entityBox.minY - pos.getY
        val minX = entityBox.minX - pos.getX
        val maxX = entityBox.maxX - pos.getX
        val minZ = entityBox.minZ - pos.getZ
        val maxZ = entityBox.maxZ - pos.getZ

        var result = -1
        var highest = Double.NegativeInfinity

        for ((part, index) <- tile.partList.zipWithIndex; box <- part.getCollisionBoxes.asScala) {
            val horizontalOverlap =
                box.max.x - IntersectionEpsilon > minX && maxX - IntersectionEpsilon > box.min.x &&
                    box.max.z - IntersectionEpsilon > minZ && maxZ - IntersectionEpsilon > box.min.z
            val supportsEntity =
                box.max.y >= footY - SupportTolerance && box.max.y <= footY + SupportTolerance

            if (horizontalOverlap && supportsEntity && box.max.y > highest) {
                highest = box.max.y
                result = index
            }
        }
        result
    }

    /**
     * Called by BlockMultipart's server-side landing hook. The normal vanilla
     * packet contains only a block-state id, which cannot identify a part.
     */
    def dispatchLandingEffects(world: WorldServer, pos: BlockPos, entity: EntityLivingBase, count: Int): Boolean = {
        BlockMultipart.getTile(world, pos) match {
            case null =>
            case tile =>
                val partIndex = findSupportPart(tile, pos, entity.getEntityBoundingBox)
                if (tile.partList.isDefinedAt(partIndex)) {
                    val part = tile.partList(partIndex)
                    MultipartSPH.queueLandingEffects(world, pos, partIndex, part.getType, entity, count)
                }
        }

        // Never fall through to vanilla BLOCK_DUST: its static multipart block
        // state cannot describe the contacted part.
        true
    }
}
