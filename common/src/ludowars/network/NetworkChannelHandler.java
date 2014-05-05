/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ludowars.network;

import ludowars.network.packets.Packet;

/**
 *
 * @author hannesmartinsson
 */
public interface NetworkChannelHandler {
    void connected();
    void received(Packet p);
}
