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
abstract public class Packet {
    abstract public void write(Output output);
    abstract public void read(Input input);

    static public int getSize() {
        throw new RuntimeException("You need to implement this.");
    }
}
