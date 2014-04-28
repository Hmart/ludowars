package ludowars.network;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryonet.EndPoint;
import ludowars.network.packets.AssignPacket;
import ludowars.network.packets.MovePacket;
import ludowars.network.packets.StatePacket;

/**
 *
 * @author kjagiello
 */
public class Network {

    static public final int port = 6666;

    static public void register(EndPoint endPoint) {
        Kryo kryo = endPoint.getKryo();
        kryo.register(MovePacket.class, 1);
        kryo.register(AssignPacket.class, 2);
        kryo.register(StatePacket.class, 3);
        
        
    }
}
