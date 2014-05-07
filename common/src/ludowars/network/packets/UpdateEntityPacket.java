/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ludowars.network.packets;

import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import ludowars.model.EntityData;

/**
 *
 * @author hannesmartinsson
 */
public class UpdateEntityPacket extends Packet{
    public EntityData ed;
    
    @Override
    public void write(Output output) {
        
    }

    @Override
    public void read(Input input) {
        EntityPacket ep = new EntityPacket();
        ep.read(input);
        ed = ep.ed;
    }
    
}
