/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ludowars.core;

import com.esotericsoftware.kryonet.Client;
import com.esotericsoftware.kryonet.Connection;
import com.esotericsoftware.kryonet.Listener;
import java.io.IOException;
import java.util.concurrent.ConcurrentLinkedQueue;
import ludowars.controller.PlayerDriver;
import ludowars.model.EntityData;
import ludowars.model.State;
import ludowars.network.Network;
import ludowars.network.packets.*;
import ludowars.view.ControlledPlayerRepresentation;

/**
 *
 * @author kjagiello
 */
public class NetworkedClient extends Client {

    public ConcurrentLinkedQueue<Object> clientMessageQueue;
    public Client client;

    public NetworkedClient() {
        super(16384, 16384);
        
        clientMessageQueue = new ConcurrentLinkedQueue<Object>();
        client = this;
        
        addListener(new Listener() {
            @Override
            public void connected(Connection connection) {
                connection.sendTCP(new Network.Connected());
            }

            @Override
            public void received(Connection c, Object object) {
                clientMessageQueue.add(object);
            }
        });
    }

    public State process(State S) {
        Object o;
        
        while ((o = clientMessageQueue.poll()) != null) {
            if (o instanceof MovePacket) {
                MovePacket mp = (MovePacket)o;
                Entity e = S.entityManager.getEntity(mp.entityID);
                
                if(e != null){
                    EntityData temp = e.getData();
                    temp.id = mp.entityID;
                    temp.position.x = mp.x;
                    temp.position.y = mp.y;               
                    e.driverStateQueue.add(mp.driverstate);
                }
                
            }
            else if (o instanceof AssignPacket) {
                AssignPacket ap = (AssignPacket)o;
                S.localPlayer = S.entityManager.getEntity(ap.id);
                S.localPlayer.setDriver(new PlayerDriver());
                S.localPlayer.setRepresentation(new ControlledPlayerRepresentation());
            }
            else if (o instanceof Network.UpdateEntity) {
                Network.UpdateEntity ue = (Network.UpdateEntity)o;
                Entity e = S.entityManager.getEntity(ue.data.id);
                
                if (e != null) {
                    e.setData(ue.data);
                }
            }
            else if (o instanceof Network.CreateEntity) {
                Network.CreateEntity ce = (Network.CreateEntity)o;
                S.entityManager.createEntity(ce.data.id, ce.data);
            }
            else if (o instanceof Network.UserCommand) {
                Network.UserCommand cmd = (Network.UserCommand)o;
                Entity e = S.entityManager.getEntity(cmd.id);
                
                if (e != null) {
                    System.out.println(e.driverStateQueue.size());
                    e.driverStateQueue.add(cmd.driverState);
                }
            }
        }
        
        if (S.localPlayer != null) {
            Network.UserCommand cmd = new Network.UserCommand();
            cmd.id = S.localPlayer.getID();
            cmd.driverState = S.localPlayer.getDriver().state;
            sendTCP(cmd);
        }
        
        return S;
    }
    
    
    private boolean connectToServer() {
        try {
            client.connect(5000, "localhost", Network.port);
            return true;
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
        return false;
    }
    
    public void connect() {
        new Thread("Connect") {
            public void run() {
                if (!connectToServer()) {
                    startLocalServer();
                    while (!connectToServer()) {}
                }
            }
        }.start();
    }
}
