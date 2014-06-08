/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ludowars.view;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.audio.Sound;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.math.Vector3;
import ludowars.model.CharacterData;
import ludowars.model.EntityData;

/**
 *
 * @author IgnatioQ
 */
public class ZombieRepresentation extends CharacterRepresentation {

    SpriteSheet handle;
    SpriteSheet tombstone;
    float health;
    long healthChangeTime;
        private boolean fire;
    private long lastFire;
    public int fireAnimationDelay = 100;

    public ZombieRepresentation() {
        Texture temp = new Texture(Gdx.files.internal("assets/images/ZombieB.png"));
        this.handle = new SpriteSheet(temp, 128, 128);
        Texture temp2 = new Texture(Gdx.files.internal("assets/images/gravestone.png"));
        this.tombstone = new SpriteSheet(temp2, 32, 32);
    }

    @Override
    public void update() {
        // Britt-Marie was here...
        super.update();
    }

    @Override
    public void render(ludowars.model.State S, SpriteBatch batch, ShapeRenderer sr, com.badlogic.gdx.graphics.OrthographicCamera camera) {
        super.update();
        
        CharacterData data = getData();
        
        float sx = data.position.x;
        float sy = data.position.y;
        Vector3 sv = new Vector3((float) sx, (float) sy, 0f);
        if (data.getHealth() > 0) {
        if (data.velocity.len() == 0f) {
                Animationlength = 4;
                
            long currentTime = System.currentTimeMillis();
            if (currentTime - lastFire > 150) {
                fire = false;
                lastFire = currentTime;
            }
             if(entity.getDriver().state.fire){
                    fire = true;
                    lastFire = currentTime;
             }
 
             if(fire){
             batch.draw(handle.grabSprite(moveAnimationFrame + 12, data.getCakeSlice()), sx - 10, sy - 6); 
            } else {
                batch.draw(handle.grabSprite(moveAnimationFrame, data.getCakeSlice()), sx - 10, sy - 6);             
            }
        }
            else {
            Animationlength = 8;
               batch.draw(handle.grabSprite(moveAnimationFrame+4, data.getCakeSlice()), sx - 10, sy - 6);
 
            }
        } else {
            Animationlength = 6;
            if(moveAnimationFrame == 5)       
               moveAnimationDelay = 100000;

            batch.draw(handle.grabSprite(moveAnimationFrame + 22, data.getCakeSlice()), sx - 10, sy - 6);
            }
        
        batch.end();
        
        if (data.getHealth() < health && healthChangeTime + 500 < System.currentTimeMillis()) {
            Sound s = Gdx.audio.newSound(Gdx.files.internal("assets/sounds/grunt.wav"));
            s.play(0.1f);
        }
        
        if (data.getHealth() != health) {
            healthChangeTime = System.currentTimeMillis();
        }
        
        health = data.getHealth();
        
        
        
        if (healthChangeTime + 2000 > System.currentTimeMillis()) {
            drawHealth(sr, sv, data);
        }
        //renderBoundingBox(sr);

        batch.begin();
    }

    public void drawHealth(ShapeRenderer s, Vector3 sv, CharacterData data) {
        if (data.getHealth() <= 0)
            return;
        
        float healthWidth = data.getHealth() / data.maxHealth * 40;
        


        if (data.getHealth() > 0) {
            s.begin(ShapeRenderer.ShapeType.Filled);
            s.setColor(Color.RED);
            s.rect(sv.x+40, sv.y + 80, healthWidth - 4, 8 - 4);
            s.end();
        }
    }

    public CharacterData getData() {
        return (CharacterData) entity.getData();
    }
}
