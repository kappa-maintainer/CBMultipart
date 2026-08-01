package codechicken.multipart

import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.particle.DigIconParticle
import codechicken.lib.vec.Vector3
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.entity.Entity
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

/** Client-only rendering half of multipart landing and running effects. */
@SideOnly(Side.CLIENT)
object MultipartParticleEffectsClient {

    def handleLandingEffects(packet: PacketCustom, world: World): Unit = {
        val pos = packet.readPos
        val partIndex = packet.readUByte
        val partType = packet.readResourceLocation
        val entityPos = packet.readVector
        val count = packet.readInt

        BlockMultipart.getClientTile(world, pos) match {
            case tile if tile != null && tile.partList.isDefinedAt(partIndex) =>
                val part = tile.partList(partIndex)
                if (part.getType == partType) {
                    particleIcon(part) match {
                        case null =>
                        case icon => addLandingParticles(world, entityPos, count, icon)
                    }
                }
            case _ =>
        }
    }

    def handleRunningEffects(world: World, pos: BlockPos, entity: Entity): Unit = {
        BlockMultipart.getClientTile(world, pos) match {
            case tile if tile != null =>
                val partIndex = MultipartParticleEffects.findSupportPart(tile, pos, entity.getEntityBoundingBox)
                if (tile.partList.isDefinedAt(partIndex)) {
                    particleIcon(tile.partList(partIndex)) match {
                        case null =>
                        case icon => addRunningParticle(world, entity, icon)
                    }
                }
            case _ =>
        }
    }

    private def particleIcon(part: TMultiPart): TextureAtlasSprite = part match {
        // Landing and running contact the upper face of the support part.
        case iconPart: TIconHitEffectsPart => iconPart.getBrokenIcon(EnumFacing.UP.getIndex)
        case _ => null
    }

    private def addLandingParticles(world: World, entityPos: Vector3, count: Int, icon: TextureAtlasSprite): Unit = {
        val manager = Minecraft.getMinecraft.effectRenderer
        val random = world.rand
        val speed = 0.15000000596046448D
        for (_ <- 0 until count) {
            manager.addEffect(DigIconParticle.newLandingParticle(
                world,
                entityPos.x,
                entityPos.y,
                entityPos.z,
                random.nextGaussian() * speed,
                random.nextGaussian() * speed,
                random.nextGaussian() * speed,
                icon))
        }
    }

    private def addRunningParticle(world: World, entity: Entity, icon: TextureAtlasSprite): Unit = {
        val random = world.rand
        val x = entity.posX + (random.nextFloat() - 0.5D) * entity.width
        val y = entity.getEntityBoundingBox.minY + 0.1D
        val z = entity.posZ + (random.nextFloat() - 0.5D) * entity.width
        Minecraft.getMinecraft.effectRenderer.addEffect(new DigIconParticle(
            world, x, y, z, -entity.motionX * 4.0D, 1.5D, -entity.motionZ * 4.0D, icon))
    }
}
