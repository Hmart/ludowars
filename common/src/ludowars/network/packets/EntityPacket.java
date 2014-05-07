/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ludowars.network.packets;

import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import ludowars.core.Entity;
import ludowars.model.EntityData;

/**
 *
 * @author hannesmartinsson
 */
public class EntityPacket extends Packet {
    public EntityData ed;
    
    public EntityPacket(){
    ed = new EntityData();
    
    }

    @Override
    public void write(Output output) {
        output.writeInt(ed.id);
        output.writeString(ed.controller);
        output.writeString(ed.representation);
        output.writeString(ed.driver);
        output.writeFloat(ed.position.x);
        output.writeFloat(ed.position.y);
        output.writeFloat(ed.velocity.x);
        output.writeFloat(ed.velocity.y);
        output.writeFloat(ed.angle);
        output.writeInt(ed.width);
        output.writeInt(ed.height);
    }

    @Override
    public void read(Input input) {
        ed.id = input.readInt();
        ed.controller = new String(input.readBytes(256)).trim();
        ed.representation = new String(input.readBytes(256)).trim();
        ed.driver = new String(input.readBytes(256)).trim();
        ed.position.x = input.readFloat();
        ed.position.y = input.readFloat();
        ed.velocity.x = input.readFloat();
        ed.velocity.y = input.readFloat();
        ed.angle = input.readFloat();
        ed.width = input.readInt();
        ed.height = input.readInt();
    }

}
