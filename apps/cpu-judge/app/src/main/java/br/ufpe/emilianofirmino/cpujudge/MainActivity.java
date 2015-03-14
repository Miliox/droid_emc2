package br.ufpe.emilianofirmino.cpujudge;

import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.NumberPicker;
import android.widget.Toast;

import java.io.DataOutputStream;


public class MainActivity extends ActionBarActivity {
    private static String TAG = "CPU_JUDGE";

    private EditText frequencySelector;
    private NumberPicker numberPicker;
    private Button runButton;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        frequencySelector = (EditText) findViewById(R.id.sel_freq);

        numberPicker = (NumberPicker) findViewById(R.id.pk_cpu_num);
        numberPicker.setMaxValue(4);
        numberPicker.setMinValue(1);
        numberPicker.setValue(1);

        runButton = (Button) findViewById(R.id.bt_cpu_cfg);
        runButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
               int cpuNum = numberPicker.getValue();

                // Stop CPU Manager Service
               String cmd = "stop mpdecision\n";

                // Disable all cpus
                cmd += "echo " + (cpuNum > 1 ? "1" : "0") + " > /sys/devices/system/cpu/cpu1/online\n";
                cmd += "echo " + (cpuNum > 2 ? "1" : "0") + " > /sys/devices/system/cpu/cpu2/online\n";
                cmd += "echo " + (cpuNum > 3 ? "1" : "0") + " > /sys/devices/system/cpu/cpu3/online\n";

                String setFreq = "echo " + frequencySelector.getText() + "000 > ";

                cmd += "echo userspace > /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor\n";
                cmd += setFreq + "/sys/devices/system/cpu/cpu0/cpufreq/scaling_min_freq\n";
                cmd += setFreq + "/sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq\n";
                cmd += setFreq + "/sys/devices/system/cpu/cpu0/cpufreq/scaling_setspeed\n";

                if (cpuNum > 1) {
                    cmd += "echo userspace > /sys/devices/system/cpu/cpu1/cpufreq/scaling_governor\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu1/cpufreq/scaling_min_freq\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu1/cpufreq/scaling_max_freq\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu1/cpufreq/scaling_setspeed\n";
                }

                if (cpuNum > 2) {
                    cmd += "echo userspace > /sys/devices/system/cpu/cpu2/cpufreq/scaling_governor\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu2/cpufreq/scaling_min_freq\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu2/cpufreq/scaling_max_freq\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu2/cpufreq/scaling_setspeed\n";
                }

                if (cpuNum > 3) {
                    cmd += "echo userspace > /sys/devices/system/cpu/cpu3/cpufreq/scaling_governor\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu3/cpufreq/scaling_min_freq\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu3/cpufreq/scaling_max_freq\n";
                    cmd += setFreq + "/sys/devices/system/cpu/cpu3/cpufreq/scaling_setspeed\n";
                }

                Log.d(TAG, cmd);

                runPrivilegedCommand(cmd);
            }
        });
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.menu_main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        // Handle action bar item clicks here. The action bar will
        // automatically handle clicks on the Home/Up button, so long
        // as you specify a parent activity in AndroidManifest.xml.
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    private void runPrivilegedCommand(String cmd) {
        boolean rooted = false;
        try {
            Process p = Runtime.getRuntime().exec("su");
            DataOutputStream os = new DataOutputStream(p.getOutputStream());
            os.writeBytes(cmd);
            os.writeBytes("\nexit\n");
            os.flush();
            p.waitFor();
            rooted = p.exitValue() != 255;
        } catch (Exception e) {
            Log.e(TAG, "Root launch an exception", e);
        }

        Toast.makeText(getApplicationContext(),
                rooted ? "SUCESS" : "FAILED", Toast.LENGTH_SHORT).show();
    }
}
