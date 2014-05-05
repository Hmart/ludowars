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
import ludowars.network.NetworkChannel;
import ludowars.network.NetworkChannelHandler;
import ludowars.network.packets.*;
import ludowars.view.ControlledPlayerRepresentation;

/**
 *
 * @author kjagiello
 */
public class NetworkedClient {

    public ConcurrentLinkedQueue<Object> clientMessageQueue;
    public NetworkChannel client;

    public NetworkedClient() {        
        clientMessageQueue = new ConcurrentLinkedQueue<Object>();
        client = new NetworkChannel();
        client.register(3, StatePacket.class);
        client.register(2, AssignPacket.class);
        
        client.setHandler(new NetworkChannelHandler() {
            @Override
            public void connected() {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void received(Packet p) {
                System.out.println(p);
                clientMessageQueue.add(p);
            }
        });
    }

    public State process(State S) {
        Object o;
        
        while ((o = clientMessageQueue.poll()) != null) {
            if(o instanceof StatePacket) {
                StatePacket p = (StatePacket)o;
                S = p.s;
                System.out.println("entity count: " + p.s.entityManager.getCount());
            }
            else if (o instanceof AssignPacket) {
                AssignPacket ap = (AssignPacket)o;
                System.out.println(ap.id);
                S.localPlayer = S.entityManager.getEntity(ap.id);
                S.localPlayer.setDriver(new PlayerDriver());
                S.localPlayer.setRepresentation(new ControlledPlayerRepresentation());
            }
               
                
                
            
            
            /*if (o instanceof MovePacket) {
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
            }*/
        }
        
        if (S.localPlayer != null) {
            /*Network.UserCommand cmd = new Network.UserCommand();
            cmd.id = S.localPlayer.getID();
            cmd.driverState = S.localPlayer.getDriver().state;
            sendTCP(cmd);*/
        }
        
        return S;
    }
    
    
    private boolean connectToServer() {
        //try {
            //client.connect(5000, "localhost", Network.port);
            client.connect("localhost", 7331);
            return true;
        //} catch (IOException ex) {
        //    ex.printStackTrace();
        //}
        
        //return false;
    }
    
    public void connect() {
        client.connect("localhost", 7331);
        /*new Thread("Connect") {
            public void run() {
                if (!connectToServer()) {
                    startLocalServer();
                    while (!connectToServer()) {}
                }
            }
        }.start();*/
    }
    public void tick(){
        
        client.read();
    
    }
}
