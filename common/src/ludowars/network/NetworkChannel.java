/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ludowars.network;

import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import ludowars.network.packets.Packet;

/**
 *
 * @author hannesmartinsson
 */
public class NetworkChannel {
    private SocketChannel channel;
    private HashMap<Integer, Class> packets;
    private Input input;
    private Output output;
    private NetworkChannelHandler handler;
    
    private ByteBuffer inputBuffer;
    // if -1, waiting for a new packet to arrive
    private int currentPacket = -1;
    private int packetLength;

    public NetworkChannel() {
        packets = new HashMap<>();
        input = new Input();
        output = new Output();
        inputBuffer = ByteBuffer.allocate(64 * 1024);
    }
    
    public void setHandler(NetworkChannelHandler _handler) {
        handler = _handler;
    }
    
    public void register(int id, Class packet) {
        if (packets.containsKey(id)) {
            throw new RuntimeException("Packet ID: " + id + " has already been registered");
        }
        
        packets.put(id, packet);
    }
    
    public void connect(String host, int port) {
        try {
            channel = SocketChannel.open();
            channel.configureBlocking(false);
            channel.connect(new InetSocketAddress(host, port));
            while(! channel.finishConnect()){
                //wait, or do stuff
            }
            
        } catch (IOException ie) {
            System.out.println("Brittmarie was here.");
        }
    }

    private void write(ByteBuffer buffer) {
        try {
            channel.write(buffer);
        } catch (IOException ie) {
            System.out.println("write exception");
        }
    }

    public void read() {
        try {
            int bytesRead = channel.read(inputBuffer);
            inputBuffer.flip();
            
            //System.out.println("Inputbuffer: " + inputBuffer);
                        
            if (bytesRead == -1) {
                System.out.println("the connection has been closed.");
                return;
            }
            
            if (inputBuffer.remaining() == 0) {
                // System.out.println("Nothing to read.");
                return;
            }
            
            if (currentPacket == -1) {
                if (inputBuffer.remaining() < 5) {
                    // nothing to read
                    return;
                }
                
                currentPacket = inputBuffer.get();
                packetLength = inputBuffer.getInt();
                
                System.out.println("CurrentPacket: " + currentPacket);
                System.out.println("PacketLength: " + packetLength);
                System.out.println("remaining: " + inputBuffer.remaining());
            }
            
            if (inputBuffer.remaining() >= packetLength) {
                // packet ready to be parsed
                Class cls = packets.get(currentPacket);
                Packet p = (Packet)cls.getConstructor().newInstance();
                Input i = new Input(inputBuffer.array());
                i.setPosition(inputBuffer.position() + inputBuffer.arrayOffset());
                p.read(i);
                handler.received(p);
                inputBuffer.position(inputBuffer.position() + packetLength);
                inputBuffer = inputBuffer.slice();
                currentPacket = -1;
            }
            
            inputBuffer.compact();
        } catch (IOException ie) {
            System.out.println("read exception");
        } catch (Throwable ex) {
            Logger.getLogger(NetworkChannel.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
}
