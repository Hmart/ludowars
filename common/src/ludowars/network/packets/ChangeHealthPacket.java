/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ludowars.network.packets;

import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;

/**
 *
 * @author hannesmartinsson
 */
public class ChangeHealthPacket extends Packet{
    public int id;
    public float health;
    
    @Override
    public void write(Output output) {
        output.writeInt(id);
        output.writeFloat(health);
    }

    @Override
    public void read(Input input) {
        id = input.readInt();
        health = input.readFloat();
    }
    
}
