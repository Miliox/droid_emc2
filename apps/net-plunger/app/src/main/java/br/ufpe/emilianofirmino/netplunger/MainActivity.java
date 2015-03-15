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

public class MainActivity extends ActionBarActivity {
    private EditText urlField;
    private Spinner  modeField;

    private NumberPicker hourPicker;
    private NumberPicker minutePicker;
    private NumberPicker secondPicker;

    private CheckBox enableRepeat;
    private EditText repeatNumber;
    private EditText packetSepTime;
    private Button   runButton;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        initializeUiComponents();
    }

    private void initializeUiComponents() {
        ArrayAdapter<CharSequence> testOptions =
                ArrayAdapter.createFromResource(
                        this, R.array.stress_mode_array, android.R.layout.simple_spinner_item);

        this.urlField      = (EditText)     findViewById(R.id.input_url);
        this.modeField     = (Spinner)      findViewById(R.id.input_test_mode);
        this.hourPicker    = (NumberPicker) findViewById(R.id.input_duration_hour);
        this.minutePicker  = (NumberPicker) findViewById(R.id.input_duration_minute);
        this.secondPicker  = (NumberPicker) findViewById(R.id.input_duration_second);
        this.enableRepeat  = (CheckBox)     findViewById(R.id.checkbox_repeat_test);
        this.repeatNumber  = (EditText)     findViewById(R.id.input_number_test_repetions);
        this.packetSepTime = (EditText)     findViewById(R.id.input_delay_between_tests);
        this.runButton     = (Button)       findViewById(R.id.button_run);

        this.modeField.setAdapter(testOptions);

        this.hourPicker.setMinValue(0);
        this.minutePicker.setMinValue(0);
        this.secondPicker.setMinValue(0);

        this.hourPicker.setMaxValue(23);
        this.minutePicker.setMaxValue(59);
        this.secondPicker.setMaxValue(59);

        this.enableRepeat.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                LinearLayout l = (LinearLayout) findViewById(R.id.repeat_field);
                l.setVisibility(isChecked ? View.VISIBLE : View.GONE);
            }
        });

        this.runButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String run = getString(R.string.execute_test);
                String end = getString(R.string.stop_test);

                if (run.equals(runButton.getText())) {
                    String url = (urlField.length() > 0)
                            ? urlField.getText().toString() : "127.0.0.1";

                    long duration = (60 * 60) * hourPicker.getValue();
                    duration += 60 * minutePicker.getValue();
                    duration += secondPicker.getValue();

                    int connType = modeField.getSelectedItemPosition();

                    int repeat = (enableRepeat.isChecked() && repeatNumber.length() > 0)
                            ? Integer.parseInt(repeatNumber.getText().toString()) : 0;

                    int delay = (enableRepeat.isChecked() && packetSepTime.length() > 0)
                            ? Integer.parseInt(packetSepTime.getText().toString()) : 0;

                    String msg = "url:" + url;
                    msg += ",dur:" + duration;
                    msg += ",conn:" + connType;

                    if (enableRepeat.isChecked()) {
                        msg += ",r:" + repeat;
                        msg += ",d:" + delay;
                    }
                    setUiComponentsEnabled(false);
                    runButton.setText(end);

                    Toast.makeText(getApplicationContext(), msg, Toast.LENGTH_LONG).show();
                }
                else if (end.equals(runButton.getText())) {
                    setUiComponentsEnabled(true);
                    runButton.setText(run);
                }
            }
        });
    }

    private void setUiComponentsEnabled(boolean enabled) {
        urlField.setEnabled(enabled);
        modeField.setEnabled(enabled);
        hourPicker.setEnabled(enabled);
        minutePicker.setEnabled(enabled);
        secondPicker.setEnabled(enabled);
        enableRepeat.setEnabled(enabled);
        repeatNumber.setEnabled(enabled);
        packetSepTime.setEnabled(enabled);
    }
}
