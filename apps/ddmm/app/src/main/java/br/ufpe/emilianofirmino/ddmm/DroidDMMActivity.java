package br.ufpe.emilianofirmino.ddmm;

import br.ufpe.emilianofirmino.ddmm.service.DroidDMMService;
import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.support.v4.content.LocalBroadcastManager;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;

import static br.ufpe.emilianofirmino.ddmm.service.DroidDMMService.*;
import static android.view.View.GONE;
import static android.view.View.VISIBLE;;

public class DroidDMMActivity extends Activity
{
    static {
        System.loadLibrary("DDMM");
    }

    private Button btnStartProfiler;
    private Button btnStopProfiler;
    private CheckBox cbPreventSleep;
    private BroadcastReceiver profilerReceiver;

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);

        btnStartProfiler = (Button) findViewById(R.id.btnStartProfiler);
        btnStopProfiler = (Button) findViewById(R.id.btnStopProfiler);
        cbPreventSleep = (CheckBox) findViewById(R.id.cbPreventSleep);

        profilerReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (intent.getAction()
                          .equals(ACTION_IS_ACTIVE)
                        && intent.getExtras()
                                 .getString(EXTRA_STATUS)
                                 .equals(STATUS_PROCESSED)) {

                    if (intent.getExtras()
                              .getBoolean(EXTRA_ACTIVE)) {
                        btnStartProfiler.setVisibility(GONE);
                        btnStopProfiler.setVisibility(VISIBLE);
                        cbPreventSleep.setEnabled(false);
                        cbPreventSleep.setChecked(
                            intent.getExtras()
                                  .getBoolean(EXTRA_PREVENT_SLEEP));
                    } else {
                        btnStartProfiler.setVisibility(VISIBLE);
                        btnStopProfiler.setVisibility(GONE);
                        cbPreventSleep.setEnabled(true);
                    }
                }

                if (intent.getAction()
                          .equals(ACTION_START)
                        && intent.getExtras()
                                 .getString(EXTRA_STATUS)
                                 .equals(STATUS_PROCESSED)) {
                    btnStartProfiler.setVisibility(GONE);
                    btnStopProfiler.setVisibility(VISIBLE);
                    cbPreventSleep.setEnabled(false);
                    cbPreventSleep.setChecked(
                        intent.getExtras()
                              .getBoolean(EXTRA_PREVENT_SLEEP));
                }

                if (intent.getAction()
                          .equals(ACTION_STOP)
                          && intent.getExtras()
                                   .getString(EXTRA_STATUS)
                                   .equals(STATUS_PROCESSED)) {
                    btnStartProfiler.setVisibility(VISIBLE);
                    btnStopProfiler.setVisibility(GONE);
                    cbPreventSleep.setEnabled(true);
                }
            }
        };

        IntentFilter filter = new IntentFilter();
        filter.addAction(ACTION_START);
        filter.addAction(ACTION_STOP);
        filter.addAction(ACTION_IS_ACTIVE);
        LocalBroadcastManager.getInstance(this)
                             .registerReceiver(profilerReceiver, filter);

        Intent serviceIntent = new Intent(this, DroidDMMService.class);
        serviceIntent.setAction(ACTION_IS_ACTIVE);
        this.startService(serviceIntent);

        final Activity activity = this;
        btnStartProfiler.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent serviceIntent = new Intent(activity, DroidDMMService.class);
                serviceIntent.setAction(ACTION_START);
                serviceIntent.putExtra(EXTRA_PREVENT_SLEEP, cbPreventSleep.isChecked());
                activity.startService(serviceIntent);
            }
        });

        btnStopProfiler.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent serviceIntent = new Intent(activity, DroidDMMService.class);
                serviceIntent.setAction(ACTION_STOP);
                activity.startService(serviceIntent);
            }
        });
    }

    public void onDestroy() {
        super.onDestroy();
        LocalBroadcastManager.getInstance(this)
                             .unregisterReceiver(profilerReceiver);
    }
}
