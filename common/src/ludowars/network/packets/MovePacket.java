/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ludowars.network.packets;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.KryoSerializable;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import ludowars.controller.EntityDriverState;

/**
 *
 * @author hannesmartinsson
 */
public class MovePacket extends Packet {
    public EntityDriverState driverstate;
    public float x;
    public float y;
    public int entityID;

    public MovePacket() {
        driverstate = new EntityDriverState();
    }

    @Override
    public void write(Output output) {
        output.writeInt(entityID);
        output.writeFloat(x);
        output.writeFloat(y);
        output.writeBoolean(driverstate.moveNorth);
        output.writeBoolean(driverstate.moveSouth);
        output.writeBoolean(driverstate.moveWest);
        output.writeBoolean(driverstate.moveEast);
        output.writeBoolean(driverstate.fire);
        output.writeBoolean(driverstate.fireSecondary);
        output.writeFloat(driverstate.mousePosition.x);
        output.writeFloat(driverstate.mousePosition.y);
    }

    @Override
    public void read(Input input) {
        entityID = input.readInt();
        x = input.readFloat();
        y = input.readFloat();
        driverstate.moveNorth = input.readBoolean();
        driverstate.moveSouth = input.readBoolean();
        driverstate.moveWest = input.readBoolean();
        driverstate.moveEast = input.readBoolean();
        driverstate.fire = input.readBoolean();
        driverstate.fireSecondary = input.readBoolean();
        driverstate.mousePosition.x = input.readFloat();
        driverstate.mousePosition.y = input.readFloat();  
    }
    
}
