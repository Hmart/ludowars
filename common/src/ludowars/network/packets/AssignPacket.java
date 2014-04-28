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

/**
 *
 * @author hannesmartinsson
 */
public class AssignPacket implements KryoSerializable{
    public int id;

    @Override
    public void write(Kryo kryo, Output output) {
        output.writeInt(id);
    }

    @Override
    public void read(Kryo kryo, Input input) {
        id = input.readInt();
        
    }
    
    
}
