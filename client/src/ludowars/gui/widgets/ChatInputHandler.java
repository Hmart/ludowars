/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ludowars.gui.widgets;

import com.badlogic.gdx.Input;
import ludowars.core.NetworkedClient;
import ludowars.network.packets.ChatPacket;
import sun.net.NetworkClient;

/**
 *
 * @author kjagiello
 */
public class ChatInputHandler extends InputHandler {
    ChatWidget chat;
    
    public ChatInputHandler(ChatWidget widget) {
        super();
        chat = widget;
    }
    
    @Override
    public boolean keyUp(int keycode) {      
        String buffer = chat.getInputBuffer();
        int bufferLength = buffer.length();
        
        if (chat.isInputActive()) {
            // closing input
            if (keycode == Input.Keys.ESCAPE) {
                chat.setInputBuffer("");
                chat.setInputActive(false);
                return true;
            }

            // submitting message
            if (keycode == Input.Keys.ENTER) {
                if (bufferLength > 0) {
                    ChatPacket p = new ChatPacket();
                    p.text = buffer.trim();
                    NetworkedClient.getInstance().client.write(p);
                }
                chat.setInputBuffer("");
                chat.setInputActive(false);
                return true;
            }
        }
        else {
            if (keycode == Input.Keys.ENTER) {
                chat.setInputActive(true);
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean keyDown(int keycode) {
        String buffer = chat.getInputBuffer();
        int bufferLength = buffer.length();
        
        if (chat.isInputActive()) {
            // deleting characters
            if (keycode == Input.Keys.BACKSPACE) {
                if (bufferLength > 0) {
                    chat.setInputBuffer(buffer.substring(0, bufferLength - 1));
                }
                return true;
            }
        }
        
        return false;
    }

    @Override
    public boolean keyTyped(char character) {
        if (chat.isInputActive() && chat.getFont().containsCharacter(character)) {
            chat.setInputBuffer(chat.getInputBuffer() + character);
            return true;
        }

        return false;
    }
}
