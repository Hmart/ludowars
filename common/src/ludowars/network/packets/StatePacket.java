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
import java.util.Collection;
import ludowars.core.Entity;
import ludowars.model.EntityData;
import ludowars.model.State;

/**
 *
 * @author hannesmartinsson
 */
public class StatePacket implements KryoSerializable {

    public State s;

    @Override
    public void write(Kryo kryo, Output output) {
        output.writeFloat(s.worldBounds.x);
        output.writeFloat(s.worldBounds.y);
        output.writeFloat(s.worldBounds.width);
        output.writeFloat(s.worldBounds.height);

        output.writeShort(s.entityManager.getCount());

        Collection<Entity> ea = s.entityManager.getAll();

        for (Entity e : ea) {
            output.writeInt(e.getID());
            output.writeString(e.getController().getClass().getCanonicalName());
            output.writeString(e.getRepresentation().getClass().getCanonicalName());
            output.writeString(e.getData().getClass().getCanonicalName());
            output.writeFloat(e.getData().position.x);
            output.writeFloat(e.getData().position.y);
            output.writeFloat(e.getData().velocity.x);
            output.writeFloat(e.getData().velocity.y);
            output.writeFloat(e.getData().angle);
            output.writeInt(e.getData().width);
            output.writeInt(e.getData().height);
        }
    }

    @Override
    public void read(Kryo kryo, Input input) {
        s.worldBounds.x = input.readFloat();
        s.worldBounds.y = input.readFloat();
        s.worldBounds.width = input.readFloat();
        s.worldBounds.height = input.readFloat();

        short entityCount = input.readShort();

        for (int i = 0; i < entityCount; i++) {
            EntityData ed = new EntityData();
            ed.id = input.readInt();
            ed.controller = input.readString();
            ed.representation = input.readString();
            ed.driver = input.readString();
            ed.position.x = input.readFloat();
            ed.position.y = input.readFloat();
            ed.velocity.x = input.readFloat();
            ed.velocity.y = input.readFloat();
            ed.angle = input.readFloat();
            ed.width = input.readInt();
            ed.height = input.readInt();
           
            s.entityManager.createEntity(ed.id, ed);
               
        }
    }

}
