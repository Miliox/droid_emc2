package br.ufpe.emilianofirmino.netplunger;

import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;
import android.widget.Toast;

import java.io.IOException;

public class MainActivity extends ActionBarActivity {
    private final String DEFAULT_URL  = "127.0.0.1";
    private final int    DEFAULT_PORT = 1234;
    private final int    DEFAULT_PACKET_SIZE = 1024;

    private EditText urlInput;
    private EditText portInput;
    private EditText packetSizeInput;
    private Spinner  transferModeOption;
    private Spinner  transferSizeOption;

    private Button   runButton;

    private PlungeClient client;
    private Thread launcher;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        initializeUiComponents();
    }

    private void initializeUiComponents() {
        this.urlInput        = (EditText)     findViewById(R.id.input_url);
        this.portInput       = (EditText)     findViewById(R.id.input_port);
        this.packetSizeInput = (EditText)     findViewById(R.id.input_packet_size);
        this.transferModeOption = (Spinner)   findViewById(R.id.input_test_mode);
        this.transferSizeOption = (Spinner)   findViewById(R.id.input_transfer_size);
        this.runButton       = (Button)       findViewById(R.id.button_run);

        this.urlInput.setHint(DEFAULT_URL);
        this.portInput.setHint(Integer.toString(DEFAULT_PORT));
        this.packetSizeInput.setHint(Integer.toString(DEFAULT_PACKET_SIZE));

        ArrayAdapter<CharSequence> testOptions = ArrayAdapter.createFromResource(
            this, R.array.stress_mode_array, android.R.layout.simple_spinner_item);
        ArrayAdapter<CharSequence> sizeOptions = ArrayAdapter.createFromResource(
                this, R.array.data_unit_array, android.R.layout.simple_spinner_item);

        this.transferModeOption.setAdapter(testOptions);

        this.runButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
            String run = getString(R.string.execute_test);
            String end = getString(R.string.stop_test);

            if (run.equals(runButton.getText())) {
                final String url = (urlInput.length() > 0)
                    ? urlInput.getText().toString()
                    : DEFAULT_URL;

                final int port = (portInput.length() > 0)
                    ? Integer.parseInt(portInput.getText().toString())
                    : DEFAULT_PORT;

                int totalDataToTransfer = (packetSizeInput.length() > 0)
                    ? Integer.parseInt(packetSizeInput.getText().toString())
                    : DEFAULT_PACKET_SIZE;

                int dataSizeOption = transferSizeOption.getSelectedItemPosition();

                if (dataSizeOption == 1) {
                    totalDataToTransfer *= 1024;
                }
                else if (dataSizeOption == 2) {
                    totalDataToTransfer *= 1024 * 1024;
                }

                int connType = transferModeOption.getSelectedItemPosition();

                String msg = "url:" + url;
                msg += ",conn:" + connType;

                setUiComponentsEnabled(false);
                runButton.setText(end);

                Toast.makeText(getApplicationContext(), msg, Toast.LENGTH_LONG).show();
                launcher = new Thread(new ClientLauncher(url, port, totalDataToTransfer, 60));
                launcher.start();
            }
            else if (end.equals(runButton.getText())) {
                if (launcher != null) {
                    synchronized (launcher) {
                        launcher.notify();
                    }
                }
            }
            }
        });
    }

    private void setUiComponentsEnabled(boolean enabled) {
        View views[] = {
            urlInput,
            portInput,
            packetSizeInput,
            transferModeOption,
            transferSizeOption
        };

        for (View v : views) {
            v.setEnabled(enabled);
        }
    }

    private class ClientLauncher implements Runnable {
        private final String url;
        private final int port;
        private final int packet;
        private final long duration;

        public ClientLauncher(String url, int port, int packet, long duration) {
            this.url = url;
            this.port = port;
            this.packet = packet;
            this.duration = duration;
        }

        @Override
        public void run() {
            client = new PlungeClient(
                PlungeClient.StressMode.FULL_DUPLEX, url, port, packet, duration);

            boolean connected = false;
            try {
                client.start();
                connected = true;
            } catch (IOException e) {
                e.printStackTrace();
            }

            final boolean isConnected = connected;
            runOnUiThread(
                new Runnable() {
                    @Override
                    public void run() {
                    if (!isConnected) {
                        setUiComponentsEnabled(true);
                        runButton.setText(getString(R.string.execute_test));
                    }
                    String message = isConnected ? "Connection Success" : "Connection Failed";
                    Toast.makeText(getApplicationContext(), message, Toast.LENGTH_LONG).show();
                    }
                }
            );

            synchronized (launcher) {
                try {
                    launcher.wait(duration * 1000);
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                    return;
                }
            }

            if (client != null) {
                client.abort();
                runOnUiThread(
                    new Runnable() {
                        @Override
                        public void run() {
                            setUiComponentsEnabled(true);
                            runButton.setText("Run");
                            String message = "Finished";
                            Toast.makeText(
                                getApplicationContext(), message, Toast.LENGTH_LONG).show();
                        }
                    }
                );
            }
            launcher = null;
        }
    };
}
