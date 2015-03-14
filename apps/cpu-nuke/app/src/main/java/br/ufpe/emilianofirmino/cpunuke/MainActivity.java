package br.ufpe.emilianofirmino.cpunuke;

import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.List;

import br.ufpe.emilianofirmino.cpunuke.tests.R;

public class MainActivity extends ActionBarActivity {
    final String CPU_ONLINE_FILE[] = new String[] {
        "/sys/devices/system/cpu/cpu0/online",
        "/sys/devices/system/cpu/cpu1/online",
        "/sys/devices/system/cpu/cpu2/online",
        "/sys/devices/system/cpu/cpu3/online"
    };

    List<SpinThread> threads;

    final long DURATION = 5 * 60 * 1000;

    final Object lock = new Object();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        final Button btnRun = (Button) findViewById(R.id.btnRun);
        btnRun.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                btnRun.setEnabled(false);
                startSpinThreads();
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

    @Override
    protected void onDestroy() {
        super.onDestroy();
        stopSpinThreads();
    }

    private void startSpinThreads() {
        stopSpinThreads();

        int numCpu = 0;
        for (String filename : CPU_ONLINE_FILE) {
            File f = new File(filename);
            try {
                FileInputStream in = new FileInputStream(f);
                if ((byte) '1' == in.read()) {
                    numCpu += 1;
                }
            } catch (Exception e) {
                // ignore
            }

            synchronized (lock) {
                threads = new ArrayList<SpinThread>(numCpu);
                for (int i = 0; i < numCpu; i++) {
                    SpinThread t = new SpinThread();
                    t.start();

                    threads.add(t);
                }
            }
        }
    }

    private void stopSpinThreads() {
        if (threads != null) {
            for (SpinThread t : threads) {
                if (t != null) {
                    t.disable();
                }
            }
        }
        threads = null;
    }

    class SpinThread extends Thread {
        private boolean active = false;

        public void run() {
            long beginTime = System.currentTimeMillis();
            active = true;
            while (active) {
                for (int i = 1; i < 1000; i++) {
                    Math.atan(Math.PI / (2 * i));

                    long now = System.currentTimeMillis();
                    long delta = now - beginTime;

                    if (delta > DURATION || !active) {
                        active = false;
                        break;
                    }
                }
            }

            synchronized (lock) {
                threads.remove(this);

                if (threads.isEmpty()) {
                    runOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            try {
                                Button btnRun = (Button) findViewById(R.id.btnRun);
                                btnRun.setEnabled(true);
                            }
                            catch (Exception e) {
                                // View Destroyed
                            }
                        }
                    });
                }
            }
        }

        public boolean isActive() {
            return active;
        }

        public void disable() {
            this.active = false;
        }
    }
}


