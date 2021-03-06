package br.ufpe.emilianofirmino.bttoolkit;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;


public class MainActivity extends ActionBarActivity {
    private BluetoothAdapter btAdapter;

    private BroadcastReceiver receiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            Button btnStartDiscovery = (Button) findViewById(R.id.btn_start_device_discovery);
            Button btnStopDiscovery  = (Button) findViewById(R.id.btn_stop_device_discovery);
            Button btnTurnVisible    = (Button) findViewById(R.id.btn_turn_visible);

            if (BluetoothAdapter.ACTION_DISCOVERY_STARTED.equals(intent.getAction())) {
                btnStartDiscovery.setEnabled(false);
                btnStopDiscovery.setEnabled(true);

                toastMessage("start device discovery ");
            } else if (BluetoothDevice.ACTION_FOUND.equals(intent.getAction())) {
                BluetoothDevice device = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);

                toastMessage("found " + device.getAddress());
            } else if (BluetoothAdapter.ACTION_DISCOVERY_FINISHED.equals(intent.getAction())) {
                btnStartDiscovery.setEnabled(true);
                btnStopDiscovery.setEnabled(false);

                toastMessage("finished device discovery ");
            } else if (BluetoothAdapter.ACTION_SCAN_MODE_CHANGED.equals(intent.getAction())) {
                int scanMode = intent.getIntExtra(BluetoothAdapter.EXTRA_SCAN_MODE, 0);
                boolean isVisible =
                    scanMode == BluetoothAdapter.SCAN_MODE_CONNECTABLE_DISCOVERABLE;
                btnTurnVisible.setEnabled(!isVisible);

                toastMessage(isVisible ? "temporary visible" : "finished visibility window");
            }
        }
    };

    private final int REQUEST_BLUETOOTH_ENABLE = 0xABC;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        registerReceiver(receiver, new IntentFilter(BluetoothAdapter.ACTION_DISCOVERY_STARTED));
        registerReceiver(receiver, new IntentFilter(BluetoothAdapter.ACTION_DISCOVERY_FINISHED));
        registerReceiver(receiver, new IntentFilter(BluetoothAdapter.ACTION_SCAN_MODE_CHANGED));
        registerReceiver(receiver, new IntentFilter(BluetoothDevice.ACTION_FOUND));

        final Button btnGetBluetoothAdapter = (Button) findViewById(R.id.btn_bt_get_adapter);
        final Button btnEnableBluetooth     = (Button) findViewById(R.id.btn_bt_enable);
        final Button btnStartDiscovery      = (Button) findViewById(R.id.btn_start_device_discovery);
        final Button btnStopDiscovery       = (Button) findViewById(R.id.btn_stop_device_discovery);
        final Button btnTurnVisible         = (Button) findViewById(R.id.btn_turn_visible);

        btnGetBluetoothAdapter.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (btAdapter == null) {
                    btAdapter = BluetoothAdapter.getDefaultAdapter();
                }

                if (btAdapter != null) {
                    v.setEnabled(false);
                    btnStopDiscovery.setEnabled(false);

                    boolean isBtEnabled = btAdapter.isEnabled();
                    btnEnableBluetooth.setEnabled(!isBtEnabled);
                    btnStartDiscovery.setEnabled(isBtEnabled);
                    btnTurnVisible.setEnabled(isBtEnabled);
                }
            }
        });

        btnEnableBluetooth.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                v.setEnabled(false);
                Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
                startActivityForResult(enableBtIntent, REQUEST_BLUETOOTH_ENABLE);
            }
        });

        btnStartDiscovery.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                btAdapter.startDiscovery();
                v.setEnabled(false);
            }
        });

        btnStopDiscovery.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                btAdapter.cancelDiscovery();
                v.setEnabled(false);
            }
        });

        btnTurnVisible.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                Intent discoverableIntent =
                    new Intent(BluetoothAdapter.ACTION_REQUEST_DISCOVERABLE);
                discoverableIntent.putExtra(BluetoothAdapter.EXTRA_DISCOVERABLE_DURATION, 30);
                startActivity(discoverableIntent);
            }
        });
    }

    @Override
    protected void onActivityResult (int requestCode, int resultCode, Intent data) {
        Button btnEnableBluetooth = (Button) findViewById(R.id.btn_bt_enable);
        Button btnFindDevices = (Button) findViewById(R.id.btn_start_device_discovery);
        Button btnTurnVisible = (Button) findViewById(R.id.btn_turn_visible);

        if (requestCode == REQUEST_BLUETOOTH_ENABLE) {
            if (resultCode == RESULT_OK) {
                btnFindDevices.setEnabled(true);
                btnTurnVisible.setEnabled(true);
            } else {
                btnEnableBluetooth.setEnabled(true);
            }
        }
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
        unregisterReceiver(receiver);
    }

    private void toastMessage(String message) {
        Toast.makeText(getApplicationContext(), message, Toast.LENGTH_SHORT).show();
    }
}
