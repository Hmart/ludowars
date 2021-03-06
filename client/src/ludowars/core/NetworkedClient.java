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
import ludowars.model.CharacterData;

/**
 *
 * @author kjagiello
 */
public class NetworkedClient {

    public ConcurrentLinkedQueue<Object> clientMessageQueue;
    public NetworkChannel client;
    private static NetworkedClient instance = null;

    private NetworkedClient() {
        clientMessageQueue = new ConcurrentLinkedQueue<Object>();
        client = new NetworkChannel();
        client.register(1, MovePacket.class);
        client.register(2, AssignPacket.class);
        client.register(3, StatePacket.class);
        client.register(4, AddEntityPacket.class);
        client.register(5, DeleteEntityPacket.class);
        client.register(6, UpdateEntityPacket.class);
        client.register(7, ChatPacket.class);
        client.register(8, DamagePacket.class);
        client.register(9, ChangeHealthPacket.class);

        client.addHandler(new NetworkChannelHandler() {
            @Override
            public void connected() {
                throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
            }

            @Override
            public void received(Packet p) {
             //System.out.println(p);
                clientMessageQueue.add(p);
            }
        });
    }
    
    public static NetworkedClient getInstance() {
        if (instance == null)
            instance = new NetworkedClient();
        return instance;
    }

    public NetworkedClient(ConcurrentLinkedQueue<Object> clientMessageQueue, NetworkChannel client) {
        this.clientMessageQueue = clientMessageQueue;
        this.client = client;
    }

    public State process(State S) {
        Object o;

        while ((o = clientMessageQueue.poll()) != null) {
            if (o instanceof StatePacket) {
                StatePacket p = (StatePacket) o;
                S = p.s;
               //  System.out.println("entity count: " + p.s.entityManager.getCount());
            } else if (o instanceof AssignPacket) {
                AssignPacket ap = (AssignPacket) o;
               // System.out.println(ap.id);
                S.localPlayer = S.entityManager.getEntity(ap.id);
                S.localPlayer.setDriver(new PlayerDriver());
                S.localPlayer.setRepresentation(new ControlledPlayerRepresentation());
            } else if (o instanceof AddEntityPacket) {
                AddEntityPacket aep = (AddEntityPacket) o;
                S.entityManager.createEntity(aep.ed.id, aep.ed);
            } else if (o instanceof DeleteEntityPacket) {
                DeleteEntityPacket dep = (DeleteEntityPacket) o;
                S.entityManager.removeEntity(dep.id);
            } else if (o instanceof UpdateEntityPacket) {
                UpdateEntityPacket uep = (UpdateEntityPacket) o;
                Entity e = S.entityManager.getEntity(uep.ed.id);
                e.setData(uep.ed);
            } else if (o instanceof ChangeHealthPacket){
                ChangeHealthPacket chp = (ChangeHealthPacket) o;
                Entity e = S.entityManager.getEntity(chp.id);    
                CharacterData cd = (CharacterData)e.getData();
                cd.health = chp.health;
                             
            } else if (o instanceof MovePacket) {
                MovePacket mp = (MovePacket) o;
                Entity e = S.entityManager.getEntity(mp.entityID);  


                if (e != null) {
                    // clear the queue if too many states to process
                    // to avoid delays
                    if (e.driverStateQueue.size() > 10) {
                        e.driverStateQueue.clear();
                    }
                    
                    mp.driverstate.position.x = mp.x;
                    mp.driverstate.position.y = mp.y;
                    e.driverStateQueue.add(mp.driverstate);
                }
            }
        }

        if (S.localPlayer!= null) {
            MovePacket movePacket = new MovePacket();
            movePacket.driverstate = S.localPlayer.getDriver().state;
            movePacket.entityID = S.localPlayer.getID();
            movePacket.x = S.localPlayer.getData().position.x;
            movePacket.y = S.localPlayer.getData().position.y;
            client.write(movePacket);
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
        //client.connect("130.238.246.5", 7331);
        /*new Thread("Connect") {
         public void run() {
         if (!connectToServer()) {
         startLocalServer();
         while (!connectToServer()) {}
         }
         }
         }.start();*/
    }

    public void tick() {

        client.read();

    }
}
