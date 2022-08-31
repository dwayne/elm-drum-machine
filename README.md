# Drum Machine

![A screenshot of the Drum Machine](/screenshot.png)

This Elm app is based on [freeCodeCamp](https://www.freecodecamp.com/)'s
[Build a Drum Machine](https://learn.freecodecamp.org/front-end-libraries/front-end-libraries-projects/build-a-drum-machine/)
front-end project. Its look and feel is "borrowed" from
[this example](https://codepen.io/freeCodeCamp/full/MJyNMd) CodePen app.

## Takeaways

I was surprised by how many new things I ended up learning and how many
interesting features of Elm I got to use.

1. I learned how to use the
[audio](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio) tag
and how to play a specific audio file when a button is clicked.
2. I got to use [ports](https://guide.elm-lang.org/interop/ports.html) as part
of enabling audio playback with the
[HTMLMediaElement](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement).
3. I figured out how to write a
[custom decoder](https://github.com/dwayne/elm-drum-machine/blob/982f76215f129e1cb80326f3191d92e18bd6c893/src/Main.elm#L179)
for key up and down events so that the keyboard could be used to control the
drum pads.
4. When the volume is changed the display is updated to show the current volume
and after a short period of time (currently 500ms) the volume information is
cleared. In order to do that I had to learn how to do the equivalent of
[setTimeout](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/setTimeout)
in Elm. This led me to discover
[Process.sleep](https://package.elm-lang.org/packages/elm/core/1.0.2/Process#sleep).
5. An empty display causes the display rectangle's height to collapse such that
when text is shown it makes the rectangle's height adjust abruptly. To fix this
problem I needed to
[make Elm output a non-breaking space](https://github.com/dwayne/elm-drum-machine/blob/982f76215f129e1cb80326f3191d92e18bd6c893/src/Main.elm#L283).
6. This is not Elm specific but I learned that setting
[currentTime](https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement/currentTime)
to `0` made the audio playback smoother when I was rapidly clicking the drum
pads.


## That's all folks!

Check out the [demo](https://dwayne.github.io/elm-drum-machine/).
