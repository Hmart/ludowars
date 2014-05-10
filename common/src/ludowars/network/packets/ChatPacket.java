/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ludowars.network.packets;

import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import java.io.UnsupportedEncodingException;
import java.util.logging.Level;
import java.util.logging.Logger;
import ludowars.core.Entity;
import ludowars.model.CharacterData;

/**
 *
 * @author hannesmartinsson
 */
public class ChatPacket extends Packet {
    public String text;
    
    public ChatPacket(){
    }

    @Override
    public void write(Output output) {
        output.writeShort(text.length());
        try {
            byte tmp[] = text.getBytes("ASCII");
            output.writeBytes(tmp);
            System.out.println(new String(tmp));
        } catch (UnsupportedEncodingException ex) {
            Logger.getLogger(ChatPacket.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public void read(Input input) {
        short length = input.readShort();
        text = new String(input.readBytes(length));
    }

}
