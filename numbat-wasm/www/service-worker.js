// The prefix is necessary if multiple web apps will be hosted at the same
// origin, e.g., as would be done on GitHub Pages. This prevents separate apps
// from clashing when it comes to origin-scoped resources such as Caches, OPFS,
// etc.
const APP_PREFIX = "numbat_";
// This should be updated any time cached assets change. It allows the entire
// set of precached files to be refreshed atomically on SW update.
// TODO: It would be nice if we did this automatically whenever a commit affects
// cached files. Note that this could also be a sequence number or similar.
// Random UUIDs just make it easier to avoid collisions without coordination.
const VERSION_UUID = "819e71eb-c3df-46b8-885e-5650feaea241";
const CACHE_NAME = `${APP_PREFIX}${VERSION_UUID}`;

// NOTE: If these lists get very large, they should be moved into Sets for
// membership tests.

// Files atomically pre-cached at installation/update.
// NOTE: We do _not_ cache the service worker itself.
// TODO: Consider self-hosting Google fonts and caching. Alternatively, we could
// cache the remote fonts directly.
const PRECACHED_FILES = [
  "./",
  "index.html",
  "index.js",
  "jquery.min.js",
  "jquery.mousewheel-min.js",
  "jquery.terminal.min.js",
  "keyboardevent-key-polyfill.js",
  "main.css",
  "opensearch.xml",
  "pkg/numbat_wasm.js",
  "pkg/numbat_wasm_bg.wasm",
  "pkg/package.json", // Does this need to be cached?
  "terminal.css",
];
// Files cached on-demand (at first load), but not refreshed. This should mostly
// include platform/device-specific resources such as icons which take up a lot
// of space and not required across the board. These should also not typically
// contain resources which would lead to semantic breakages if out of sync with
// other files.
const CACHED_FILES = [
  "icon.svg",
  "numbat-16x16.png",
  "numbat-196x196.png",
  "numbat-32x32.png",
  "numbat.png",
  "numbat.svg",
  // TODO: Google Fonts? Self-host and pre-cache those?
];
// URLs for which we hit the internet on every request, only falling back to the
// cache when offline.
const DYNAMIC_URLS = ["https://numbat.dev/ecb-exchange-rates.php"];

const getAbsoluteUrl = (file) => new URL(file, self.location.href).href;

const PRECACHED_URLS = PRECACHED_FILES.map(getAbsoluteUrl);
const CACHED_URLS = CACHED_FILES.map(getAbsoluteUrl);

const handleInstall = async (e) => {
  e.waitUntil(
    (async () => {
      // Atomically pre-cache common assets. This is technically racy because
      // there's no coordination between SW version id and cached file
      // contents (that would require something like asset hashes). However,
      // the time window for such a race is pretty narrow and unlikely to
      // arise in practice.
      const cache = await caches.open(CACHE_NAME);
      await cache.addAll(PRECACHED_FILES);
      console.log(`[Service Worker] installed version ${VERSION_UUID}`);
    })()
  );
};

const handleActivate = async (e) => {
  e.waitUntil(
    (async () => {
      // Drop old cache versions so storage doesn't explode. By doing this
      // only at activation time, we ensure that older versions of the app
      // continue to work until the new one takes over.
      const keys = await caches.keys();
      const deletions = [];
      for (const key of keys) {
        if (!key.startsWith(APP_PREFIX)) {
          // Only touch caches related to this app, even if they're at the
          // same origin.
          continue;
        }
        if (key != CACHE_NAME) {
          // Only keep the most recent cache version.
          deletions.push(caches.delete(key));
        }
      }
      // NOTE: It might be excessive to fail activation on cleanup of old
      // caches, but something has clearly gone wrong in this case.
      await Promise.all(deletions);
    })()
  );
};

const handleFetch = async (e) => {
  const url = new URL(e.request.url).href;

  const servePrecachedFile = async () => {
    const cache = await caches.open(CACHE_NAME);
    // NOTE: It may be possible for our caches to be evicted, in which case
    // we'd need to re-fetch. On the other hand, this also breaks the atomic
    // update semantics and could lead to mismatched asset versions and
    // subtle breakage.

    // NOTE: The await isn't strictly necessary here thanks to Promise
    // coalescing, but this is less error-prone if we add more logic below.
    // Same applies elsewhere.
    return await cache.match(e.request);
  };

  const serveCachedFile = async () => {
    const cache = await caches.open(CACHE_NAME);
    const cachedResponse = await cache.match(e.request);
    if (cachedResponse) {
      // Serve the cached resource and don't attempt to refresh.
      return cachedResponse;
    }
    // The requested icon was not in the cache. Fetch it and return it, but
    // also add it to the cache for later.
    let fetchedResponse;
    try {
      fetchedResponse = await fetch(e.request);
    } catch (error) {
      console.error(
        `[Service Worker] failed to fetch cached resource at ${url}`,
        error
      );
    }
    if (fetchedResponse && fetchedResponse.ok) {
      // Asynchronously extend this event handler but don't wait to return
      // the response. We also don't cache failure values.
      e.waitUntil(cache.put(url, fetchedResponse.clone()));
    }
    return fetchedResponse;
  };

  const serveDynamicUrl = async () => {
    // We want fresh data for this resource. Try to hit the internet first,
    // falling back to cache on failure.
    let fetchedResponse;
    try {
      fetchedResponse = await fetch(e.request);
    } catch (error) {
      console.error(`[Service Worker] error fetching ${url}`, error);
    }
    if (fetchedResponse && fetchedResponse.ok) {
      e.waitUntil(cache.put(url, fetchedResponse.clone()));
      return fetchedResponse;
    }
    console.error(
      `[Service Worker] failed to fetch dynamic resource at ${url}; falling back to cache`
    );
    return await cache.match(e.request);
  };

  if (PRECACHED_URLS.includes(url)) {
    // Respond with precached asset. We don't hit the internet for these
    // until the app is next updated.
    e.respondWith(servePrecachedFile());
    return;
  }

  if (CACHED_URLS.includes(url)) {
    e.respondWith(serveCachedFile());
    return;
  }

  if (DYNAMIC_URLS.includes(url)) {
    e.respondWith(serveDynamicUrl());
    return;
  }
};

self.addEventListener("install", handleInstall);
self.addEventListener("activate", handleActivate);
self.addEventListener("fetch", handleFetch);
