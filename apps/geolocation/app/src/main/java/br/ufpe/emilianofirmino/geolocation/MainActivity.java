package br.ufpe.emilianofirmino.geolocation;

import android.content.Context;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;


public class MainActivity extends ActionBarActivity implements LocationListener {
    private LocationManager lm;

    private TextView altitudeTextView;
    private TextView latitudeTextView;
    private TextView longitudeTextView;
    private TextView providerTextView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        altitudeTextView = (TextView) findViewById(R.id.text_altitude);
        latitudeTextView = (TextView) findViewById(R.id.text_latitude);
        longitudeTextView = (TextView) findViewById(R.id.text_longitude);
        providerTextView = (TextView) findViewById(R.id.text_provider);

        lm = (LocationManager) getSystemService(Context.LOCATION_SERVICE);
        lm.requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, this);

        Location location = lm.getLastKnownLocation(LocationManager.GPS_PROVIDER);
        if (location != null) {
            altitudeTextView.setText(getString(R.string.text_altitude) + location.getAltitude());
            latitudeTextView.setText(getString(R.string.text_latitude) + location.getLatitude());
            longitudeTextView.setText(getString(R.string.text_longitude) + location.getLongitude());
        }

        if (lm.isProviderEnabled(LocationManager.GPS_PROVIDER)) {
            providerTextView.setText(getString(R.string.text_provider) + "ENABLE");
        } else {
            providerTextView.setText(getString(R.string.text_provider) + "DISABLE");
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
    public void onLocationChanged(final Location location) {
        runOnUiThread(new Runnable() {
            @Override
            public void run() {
                altitudeTextView.setText(getString(R.string.text_altitude) + location.getAltitude());
                latitudeTextView.setText(getString(R.string.text_latitude) + location.getLatitude());
                longitudeTextView.setText(getString(R.string.text_longitude) + location.getLongitude());
            }
        });
    }

    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {
        Log.d("GEOLOCATION","onStatusChanged: provider:" + provider + ",status:" + status);
    }

    @Override
    public void onProviderEnabled(String provider) {

    }

    @Override
    public void onProviderDisabled(String provider) {

    }
}
