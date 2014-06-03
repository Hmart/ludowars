package ludowars.controller;

import com.badlogic.gdx.graphics.Camera;
import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.math.Rectangle;
import com.badlogic.gdx.math.Vector2;
import com.badlogic.gdx.math.Vector3;
import java.util.ArrayList;
import ludowars.controller.CharacterController;
import ludowars.controller.EntityDriverState;
//import ludowars.controller.PlayerDriver;
import ludowars.core.Entity;
//import ludowars.core.GameListener;
import ludowars.model.EntityData;
import ludowars.model.ProjectileData;
import ludowars.model.State;
//import ludowars.view.LudoRepresentation;
//import ludowars.view.PowerRepresentation;
import ludowars.network.Network;
import ludowars.model.CharacterData;

/**
 *
 * @author Ignatio
 */
public class ZombieController extends CharacterController {
    public ZombieController() {
        super();
    }
    
    @Override
    public void update(State S, float delta) {
        EntityData data = entity.getData();
        EntityDriverState driverState = getDriver().state;

        //Calculate Angle
        data.angle = getMouseAngle();
        
        if (driverState.fire) {
            Vector2 attackVector = data.position.cpy();
            attackVector.add(16.0f, 16.0f).setAngle(entity.getData().angle);
            Rectangle attackZone = new Rectangle(attackVector.x - 16.0f, attackVector.y - 16.0f, 32.0f, 32.0f);
            ArrayList<Entity> entities = S.entityManager.getEntities(attackZone);
            
            for (Entity e : entities) {
                EntityData d = e.getData();
                
                if (d instanceof CharacterData) {
                    CharacterData cd = (CharacterData)d;
                    cd.changeHealth(-30.0f);
                }
            }
        }
        
        entity.setFlag(entity.ENTITY_MOVED);
        //System.out.println(getCollidingEntities(S));
        
        Vector2 oldPosition = data.position.cpy();
        Vector2 oldVelocity = data.velocity.cpy();
        
        applyAcceleration();
        applyVelocity(delta);
        applyDrag();
    }

    private EntityDriver getDriver() {
        return entity.getDriver();
    }

    protected float getMouseAngle() {
        return getAngle(getDriver().state.mousePosition);
    }

    protected float getAngle(Vector2 position) {
        return entity.getData().position.cpy().sub(position).scl(-1f).angle();
    }
    private void applyAcceleration() {
        entity.getData().velocity.add(entity.getDriver().getAccelerationVector().nor().scl(75));
    }

    private void applyVelocity(float delta) {
        entity.getData().position.x += this.entity.getData().velocity.x * delta;
        entity.getData().position.y += this.entity.getData().velocity.y * delta;
    }

    private void applyDrag() {
        entity.getData().velocity.scl(0.50f);
    }
}
