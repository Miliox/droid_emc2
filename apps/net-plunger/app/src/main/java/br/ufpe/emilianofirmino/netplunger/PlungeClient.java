package br.ufpe.emilianofirmino.netplunger;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.Random;

public class PlungeClient {
    final private StressMode mode;
    final private String     url;
    final private int        port;
    final private int        payloadSize;
    final private long       packetDueTime;

    private byte[] payload;

    private Socket conn;
    private InputStream in;
    private OutputStream out;
    private Thread       reader;
    private Thread       writer;

    public PlungeClient(
            StressMode stressMode,
            String     serverUrl,
            int        serverPort,
            int        payloadSizeInBytes,
            long       packetDueTimeInMilliseconds) {

        this.mode = stressMode;
        this.url  = serverUrl;
        this.port = serverPort;

        this.payloadSize   = payloadSizeInBytes;
        this.packetDueTime = packetDueTimeInMilliseconds;

        this.payload = new byte[this.payloadSize];

        Random r = new Random();
        r.nextBytes(this.payload);
    }

    public void start() throws IOException, IllegalStateException {
        if (this.conn != null) {
            throw new IllegalStateException("Session in progress, abort or try later");
        }

        try {
            this.conn = new Socket(url, port);
        } catch (IOException e) {
            this.conn = null;
            throw e;
        }

        this.in = conn.getInputStream();
        this.out = conn.getOutputStream();

        this.reader = new Thread(new Runnable() {
            @Override
            public void run() {
                readerLoop();
            }
        });

        this.writer = new Thread(new Runnable() {
            @Override
            public void run() {
                writerLoop();
            }
        });

        this.reader.start();
        this.writer.start();
    }

    public void abort() {
        try {
            this.in.close();
            this.out.close();
            this.conn.close();
            this.conn = null;
        } catch (Exception e) {

        }
    }

    private void readerLoop() {
        if (mode == StressMode.SIMPLEX_TX) {
            return;
        }

        byte[] buffer = new byte[this.payloadSize];
        for(;;) {
            try {
                in.read(buffer);
            } catch (IOException e) {
                return;
            }
        }
    }

    private void writerLoop() {
        if (mode == StressMode.SIMPLEX_RX) {
            return;
        }

        for(;;) {
            try {
                out.write(this.payload);
                out.flush();
                Thread.sleep(this.packetDueTime);
            } catch (Exception e) {
                return;
            }
        }
    }

    public enum StressMode {
        SIMPLEX_RX, SIMPLEX_TX, HALF_DUPLEX, FULL_DUPLEX
    }
}
