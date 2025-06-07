>module Physics.Music where
>import Math.Number.DimensionalAnalysis

>root_note o = o*12

>major_chord k o = (root_note o + k, root_note o + 4 + k, root_note o + 7 + k)
>minor_chord k o = (root_note o + k, root_note o + 3 + k, root_note o + 7 + k)
>diminished_chord k o = (root_note o + k, root_note o + 3 + k, root_note o + 6 + k)
>sus4_chord k o = (root_note o + k, root_note o + 5 + k, root_note o + 7 + k)

chord_root uses the circle of fifths to find a root note of a chord
according to progression.

>chord_root :: Int -> Int
>chord_root i = (i*7) `mod` 12

>major_progression i o = major_chord (chord_root i) o
>minor_progression i o = minor_chord (chord_root i) o
>dimished_progression i o = diminished_chord (chord_root i) o
>sus4_progression i o = sus4_chord (chord_root i) o
