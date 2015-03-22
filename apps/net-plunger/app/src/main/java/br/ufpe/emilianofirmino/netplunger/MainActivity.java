package br.ufpe.emilianofirmino.netplunger;

import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.CompoundButton;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.NumberPicker;
import android.widget.Spinner;
import android.widget.Toast;

import java.io.IOException;

public class MainActivity extends ActionBarActivity {
    private final String DEFAULT_URL  = "127.0.0.1";
    private final int    DEFAULT_PORT = 1234;
    private final int    DEFAULT_PACKET_SIZE = 1024;

    private final int HOUR_TO_SECOND = 3600;
    private final int MINUTE_TO_SECOND = 60;

    private final int LAST_HOUR   = 23;
    private final int LAST_MINUTE = 59;
    private final int LAST_SECOND = 59;

    private EditText urlInput;
    private EditText portInput;
    private EditText packetSizeInput;
    private Spinner  transferOption;

    private NumberPicker hourSelector;
    private NumberPicker minuteSelector;
    private NumberPicker secondSelector;

    private CheckBox repeatCheckBox;
    private EditText repeatNumber;
    private EditText packetSepTime;
    private Button   runButton;

    private PlungeClient client;

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
        this.transferOption  = (Spinner)      findViewById(R.id.input_test_mode);
        this.hourSelector    = (NumberPicker) findViewById(R.id.input_duration_hour);
        this.minuteSelector  = (NumberPicker) findViewById(R.id.input_duration_minute);
        this.secondSelector  = (NumberPicker) findViewById(R.id.input_duration_second);
        this.repeatCheckBox  = (CheckBox)     findViewById(R.id.checkbox_replay_test);
        this.repeatNumber    = (EditText)     findViewById(R.id.input_replay_tries);
        this.packetSepTime   = (EditText)     findViewById(R.id.input_replay_delay);
        this.runButton       = (Button)       findViewById(R.id.button_run);

        this.urlInput.setHint(DEFAULT_URL);
        this.portInput.setHint(Integer.toString(DEFAULT_PORT));
        this.packetSizeInput.setHint(Integer.toString(DEFAULT_PACKET_SIZE));

        ArrayAdapter<CharSequence> testOptions = ArrayAdapter.createFromResource(
            this, R.array.stress_mode_array, android.R.layout.simple_spinner_item);
        this.transferOption.setAdapter(testOptions);

        this.hourSelector.setMaxValue(LAST_HOUR);
        this.minuteSelector.setMaxValue(LAST_MINUTE);
        this.secondSelector.setMaxValue(LAST_SECOND);

        this.repeatCheckBox.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                LinearLayout l = (LinearLayout) findViewById(R.id.replay_field);
                l.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            }
        });

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

                final int packetSize = (packetSizeInput.length() > 0)
                    ? Integer.parseInt(packetSizeInput.getText().toString())
                    : DEFAULT_PACKET_SIZE;

                long duration = hourSelector.getValue() * HOUR_TO_SECOND;
                duration += minuteSelector.getValue() * MINUTE_TO_SECOND;
                duration += secondSelector.getValue();

                int connType = transferOption.getSelectedItemPosition();

                int repeat = (repeatCheckBox.isChecked() && repeatNumber.length() > 0)
                    ? Integer.parseInt(repeatNumber.getText().toString())
                    : 0;

                int delay = (repeatCheckBox.isChecked() && packetSepTime.length() > 0)
                    ? Integer.parseInt(packetSepTime.getText().toString())
                    : 0;

                String msg = "url:" + url;
                msg += ",dur:" + duration;
                msg += ",conn:" + connType;

                if (repeatCheckBox.isChecked()) {
                    msg += ",r:" + repeat;
                    msg += ",d:" + delay;
                }
                setUiComponentsEnabled(false);
                runButton.setText(end);

                Toast.makeText(getApplicationContext(), msg, Toast.LENGTH_LONG).show();
                new Thread(new ClientLauncher(url, port, packetSize, duration)).start();
            }
            else if (end.equals(runButton.getText())) {
                setUiComponentsEnabled(true);
                runButton.setText(run);
                if (client != null) {
                    client.abort();
                }
            }
            }
        });
    }

    private void setUiComponentsEnabled(boolean enabled) {
        View views[] = {
            urlInput,
            transferOption,
            hourSelector,
            minuteSelector,
            secondSelector,
            repeatCheckBox,
            repeatNumber,
            packetSepTime
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
        }
    };
}
