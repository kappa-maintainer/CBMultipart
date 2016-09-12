package codechicken.multipart.minecraft;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.CuboidRayTraceResult;
import codechicken.lib.render.TextureUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.multipart.BlockMultipart;
import codechicken.multipart.IModelRenderPart;
import codechicken.multipart.IconHitEffects;
import net.minecraft.block.state.BlockStateContainer;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.particle.ParticleManager;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.BlockRenderLayer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

public abstract class McMetaPart extends McBlockPart implements IModelRenderPart
{
    public IBlockState state;

    public McMetaPart()
    {
    }

    public McMetaPart(IBlockState state)
    {
        this.state = state;
    }

    @Override
    public void save(NBTTagCompound tag)
    {
        tag.setByte("meta", getMeta());
    }

    @Override
    public void load(NBTTagCompound tag)
    {
        setMeta(tag.getByte("meta"));
    }

    @Override
    public void writeDesc(MCDataOutput packet)
    {
        packet.writeByte(getMeta());
    }

    @Override
    public void readDesc(MCDataInput packet)
    {
        setMeta(packet.readByte());
    }

    public byte getMeta()
    {
        return (byte) getBlock().getMetaFromState(state);
    }

    public void setMeta(byte meta)
    {
        state = getBlock().getStateFromMeta(meta);
    }

    @Override
    public ResourceLocation getModelPath()
    {
        return null;
    }

    @Override
    public boolean canRenderInLayer(BlockRenderLayer layer)
    {
        return getBlock().canRenderInLayer(state, layer);
    }

    @Override
    public BlockStateContainer createBlockStateContainer()
    {
        return getBlock().getBlockState();
    }

    @Override
    public IBlockState getCurrentState(IBlockState state)
    {
        return this.state;
    }

    public void setStateOnPlacement(World world, BlockPos pos, EnumFacing facing, Vec3d hitVec, EntityLivingBase placer)
    {
        state = getBlock().onBlockPlaced(world, pos, facing, (float)hitVec.xCoord, (float)hitVec.yCoord, (float)hitVec.zCoord, 0, placer);
    }

    @Override
    public float getStrength(EntityPlayer player, CuboidRayTraceResult hit)
    {
       return BlockMultipart.getStrength(player, state);
    }

    @Override
    public int getLightValue()
    {
        return state.getLightValue();
    }

    @Override
    public Cuboid6 getBounds()
    {
        if (tile() != null)
            return new Cuboid6(getBlock().getBoundingBox(state, world(), pos()));
        else
            return new Cuboid6(getBlock().getBoundingBox(state, null, null));
    }

    @Override
    @SideOnly(Side.CLIENT)
    public TextureAtlasSprite getBreakingIcon(CuboidRayTraceResult hit)
    {
        return getBrokenIcon(hit.sideHit.ordinal());
    }

    @Override
    @SideOnly(Side.CLIENT)
    public TextureAtlasSprite getBrokenIcon(int side)
    {
        return TextureUtils.getParticleIconForBlock(state);
    }

    @Override
    public void addHitEffects(CuboidRayTraceResult hit, ParticleManager manager)
    {
        IconHitEffects.addHitEffects(this, hit, manager);
    }

    @Override
    public void addDestroyEffects(CuboidRayTraceResult hit, ParticleManager manager)
    {
        IconHitEffects.addDestroyEffects(this, manager, false);
    }
}
