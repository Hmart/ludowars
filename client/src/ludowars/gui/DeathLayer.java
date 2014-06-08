/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ludowars.gui;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.audio.Sound;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.math.Interpolation;
import com.badlogic.gdx.math.Vector2;
import ludowars.core.NetworkedClient;
import ludowars.core.gui.FontManager;
import ludowars.core.gui.Layer;
import ludowars.gui.widgets.ChatWidget;
import ludowars.gui.widgets.ImageView;
import ludowars.gui.widgets.InputHandler;
import ludowars.gui.widgets.SpriteSheetView;
import ludowars.model.CharacterData;
import ludowars.model.State;
import ludowars.network.NetworkChannelHandler;
import ludowars.network.packets.ChangeHealthPacket;
import ludowars.network.packets.ChatPacket;
import ludowars.network.packets.Packet;
import ludowars.view.SpriteSheet;

/**
 *
 * @author kjagiello
 */
public class DeathLayer extends Layer {

    SpriteSheet misc;
  //  ImageView heart, weapon, backpack, backpack_open;
    ChatWidget chat;
    BitmapFont font;
    String health = new String();
    float oldHealth, currentHealth, targetHealth;
    long healthTimer;
    float animationTime;
    boolean bpClickedOut = false;
    float pos = 0;
    long last = 0;
    long current = 0;
    Vector2 backpackPosition = new Vector2(0, 0);

    static final float HEALTH_ANIMATION_TIME = 2f;

    public DeathLayer() {
    }

    @Override
    public void create() {
        setZIndex(1);

       
        addWidget(chat = new ChatWidget());
        
        NetworkedClient.getInstance().client.addHandler(new NetworkChannelHandler() {
            @Override
            public void received(Packet p) {
                if (p instanceof ChatPacket) {
                    ChatPacket cp = (ChatPacket)p;
                    chat.addLine(cp.text);
                }

            }

            @Override
            public void connected() {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }
        });
        
        chat.setPosition(20, 80);
        font = FontManager.getInstance().getFont("Minecraftia.ttf", 36);
    }

    @Override
    public void show() {

    }

    @Override
    public void hide() {

    }

    @Override
    public void render(SpriteBatch batch, ShapeRenderer sr, float delta) {
        super.render(batch, sr, delta);

        // animated health display
        if (animationTime <= HEALTH_ANIMATION_TIME) {
            animationTime = Math.min(animationTime + delta, HEALTH_ANIMATION_TIME);
            currentHealth = oldHealth + (targetHealth - oldHealth) * Interpolation.exp5Out.apply(animationTime / HEALTH_ANIMATION_TIME);
            health = "" + (int) currentHealth;
        }

        float textHeight = font.getBounds("100").height;

        font.setColor(Color.BLACK);
      
    }

    @Override
    public void resize(int width, int height) {
        super.resize(width, height);

   
    }

    @Override
    public void update(State S) {
        if (S.localPlayer != null) {
            CharacterData p = (CharacterData) S.localPlayer.getData();

            if (targetHealth != p.getHealth()) {
                animationTime = 0;
                targetHealth = p.getHealth();
                oldHealth = currentHealth;
            } else if (animationTime == HEALTH_ANIMATION_TIME) {
                oldHealth = currentHealth;
            }
        }
    }
}
