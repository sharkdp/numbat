Build:
``` bash
wasm-pack build
```

Test:
``` bash
wasm-pack test --headless --firefox
```

Serve:
```bash
export NODE_OPTIONS=--openssl-legacy-provider # workaround for https://github.com/webpack/webpack/issues/14532
cd www
npm run start
```

Deploy:
```bash
bash deploy.sh
```
