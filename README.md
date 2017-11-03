# Othello

[ClojureScript](https://clojurescript.org/) implementation of Othello per Peter Norvig's [_Paradigms of Artificial Intelligence Programming_](https://norvig.com/paip.html) (PAIP), Chapter 18.

## Usage

### Development mode

To start the Figwheel compiler, navigate to the project folder and run the following command in the terminal:

```
lein figwheel
```

### Building for production

```
lein clean
lein package
```

## Improvements

- [ ] Populate `edge-table` more quickly
  - How best to precalculate and load?
- [ ] Implement `alpha-beta-3`, per section 18.11
- [ ] Improve performance
  - 3-ply Iago is pretty fast, 4-ply starts to be slow (though faster near the end where possibilities are reduced).
  - Is the issue algorithmic, or could constant-factor improvements like [advanced CLJS compilation](https://clojurescript.org/reference/advanced-compilation) help?
  - If algorithmic, consider improvements on α-β pruning like [negamax](https://en.wikipedia.org/wiki/Negamax).
- [ ] Better UI
- [ ] Better strategies (latest literature)
