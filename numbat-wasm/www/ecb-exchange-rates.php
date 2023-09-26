<?php
$cacheFile = '/tmp/numbat-exchange-rates-cache.xml';
$cacheTime = 24 * 60 * 60; // 24 hours in seconds
$ecbUrl = 'https://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml';

if (file_exists($cacheFile) && (time() - filemtime($cacheFile) < $cacheTime)) {
    // If cache file is less than 24 hours old, serve it
    header('Content-Type: text/xml');
    echo file_get_contents($cacheFile);
} else {
    // Else, fetch fresh data and cache it
    $xmlData = file_get_contents($ecbUrl);

    if ($xmlData) {
        file_put_contents($cacheFile, $xmlData); // Cache the fetched data
        header('Content-Type: text/xml');
        echo $xmlData; // Serve the fetched data
    } else {
        // Handle failure in fetching data (e.g., serve cached data even if older than 24 hours or serve an error message)
        header('HTTP/1.1 500 Internal Server Error');
        echo "Error fetching the exchange rates.";
    }
}
?>
