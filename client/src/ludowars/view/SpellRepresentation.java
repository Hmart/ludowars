/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package ludowars.view;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import ludowars.model.ProjectileData;
import ludowars.model.CharacterData;

/**
 *
 * @author kjagiello
 */
public class SpellRepresentation extends EntityRepresentation {
    SpriteSheet sprite;
    protected int moveAnimationFrame;
    private long moveAnimationTimer;
    public int moveAnimationDelay = 100;
    public static final int CHARACTER_IDLE_ANIMATION = 0;
    public int Animationlength=1;
    
    public SpellRepresentation() {
        Texture temp = new Texture(Gdx.files.internal("assets/images/Spell.png"));
        sprite = new AnimatedSpriteSheet(temp, 64, 64);
        
   
        
    }
  
    
    @Override
    public void update() {
        
    }
    
    @Override
    public void render(ludowars.model.State S, SpriteBatch batch, ShapeRenderer sr, com.badlogic.gdx.graphics.OrthographicCamera camera) {
                long currentTime = System.currentTimeMillis();

        if ((currentTime - moveAnimationTimer) > moveAnimationDelay) {
            moveAnimationFrame = ++moveAnimationFrame % 8;
            moveAnimationTimer = currentTime;
        }    
        
        batch.draw(sprite.grabSprite(moveAnimationFrame, entity.getData().getCakeSlice()), entity.getData().position.x+10, entity.getData().position.y+25);    }
}
