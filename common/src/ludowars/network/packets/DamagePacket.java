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
public class DamagePacket extends Packet{
    public float damage;
    public int targetID;
    public int sourceID;
    
    @Override
    public void write(Output output) {
        output.writeFloat(damage);
        output.writeInt(targetID);
        output.writeInt(sourceID);
    }

    @Override
    public void read(Input input) {
    }
    
}
