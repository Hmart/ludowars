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
import java.io.IOException;
import java.math.BigInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author hannesmartinsson
 */
public class AssignPacket extends Packet {
    public int id;

    @Override
    public void write(Output output) {
        output.writeInt(id);
    }

    @Override
    public void read(Input input) {
        id = input.readInt();
    }
}
