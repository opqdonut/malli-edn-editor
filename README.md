# malli-edn-editor

A web editor for Clojure data (or [EDN](https://github.com/edn-format/edn))
that has a [malli](https://github.com/metosin/malli) schema.

Probably not usable for your use case as-is, but more of a source of
inspiration for malli-driven UIs.

[Jump in and read the code!](./src/editor.cljs)

Run it yourself:
```
npm ci
npx shadow-cljs watch frontend
# navigate to <http://localhost:8888>
```

[Live demo](https://opqdonut.github.io/malli-edn-editor/)

![Screenshot](./screenshot.png)

## FAQ

- Can I change the schema?
  - Edit `example-schema` in `editor.cljs` and see your changes live
