package codechicken.multipart.scalatraits

import java.util.{LinkedList => JLinkedList}

import codechicken.multipart.{TMultiPart, TileMultipart}
import net.minecraft.util.ITickable

import scala.collection.JavaConverters._

/**
 * Mixin Trait for parts implementing ITickable. Allows parts
 * to receive update calls every tick.
 */
trait TTickableTile extends TileMultipart with ITickable {

    private var doesTick = false
    var tickingParts = new JLinkedList[TMultiPart & ITickable]() //cache to reduce iteration of all parts

    override def copyFrom(that: TileMultipart): Unit = {
        super.copyFrom(that)
        that match {
            case tile: TTickableTile => {
                tickingParts = tile.tickingParts
                doesTick = tile.doesTick
            }
            case _ =>
        }
    }

    override def bindPart(part: TMultiPart): Unit = {
        super.bindPart(part)
        part match {
            case tickingPart: ITickable =>
                tickingParts.asScala += tickingPart
                setTicking(true)
            case _ =>
        }
    }

    override def partRemoved(part: TMultiPart, p: Int): Unit = {
        super.partRemoved(part, p)
        part match {
            case tickingPart: ITickable => tickingParts.asScala -= tickingPart
            //No need to disable ticking if empty, this tile will be replaced w/ nonticking one anyway
            case _ =>
        }
    }

    override def clearParts(): Unit = {
        super.clearParts()
        tickingParts.clear()
    }

    override def update(): Unit = {
        val it = tickingParts.iterator
        while (it.hasNext) {
            val p = it.next()
            if (p.tile != null) p.update()
        }
    }

    override def loadFrom(that: TileMultipart): Unit = {
        super.loadFrom(that)
        //Add the new tile to the ticking list.
        if (doesTick) {
            getWorld.tickableTileEntities.add(this)
        }
    }

    override def loadTo(that: TileMultipart): Unit = {
        super.loadTo(that)
        //Remove the old tile from the ticking list.
        if (doesTick) {
            getWorld.tickableTileEntities.remove(this)
        }
    }

    private def setTicking(tick: Boolean): Unit = {
        if (doesTick == tick) return
        doesTick = tick

        if (getWorld != null && getWorld.getTileEntity(getPos) == this) {
            if (tick) {
                getWorld.addTileEntity(this)
            } else {
                getWorld.tickableTileEntities.remove(this)
            }
        }
    }
}

/**
 * To handle obfuscation issues, this is registered as a java trait.
 */
class JTickableTile extends TileMultipart with TTickableTile {
}
