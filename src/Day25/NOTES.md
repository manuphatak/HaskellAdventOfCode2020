## NOTES

`transform subjectNumber loopSize` appears to be `subjectNumber ^ loopSize mod 20201227`

https://www.wolframalpha.com/input/?i=%287+%5E+x%29+mod+20201227

```hs
(7 ^ x) mod 20201227
x = floor (log 20201227 / log 7)
```

### HandShake steps

```hs

-- cardsPublicKey' = transform 7 <cardsSecretLoopSize>
-- doorsPublicKey' = transform 7 <doorsSecretLoopSize>

-- <cardsSecretLoopSize> = crack 7 cardsPublicKey
-- <doorsSecretLoopSize> = crack 7 doorsPublicKey

-- encryptionKey = transform doorsPublicKey <cardsSecretLoopSize>
-- encryptionKey = transform cardsPublicKey <doorsSecretLoopSize>

-- encryptionKey = transform doorsPublicKey (crack 7 cardsPublicKey)
-- encryptionKey = transform cardsPublicKey (crack 7 doorsPublicKey)
```

```hs

cardsPublicKey :: Int
cardsPublicKey = 5764801

doorsPublicKey :: Int
doorsPublicKey = 17807724

>>> transform 7 8
5764801

>>> crack 7 cardsPublicKey
8

>>> transform 7 11
17807724

>>> crack 7 doorsPublicKey
11

>>> transform doorsPublicKey (crack 7 cardsPublicKey)
14897079

>>> transform cardsPublicKey (crack 7 doorsPublicKey)
14897079
```

## Seed values

I still don't know how to solve equations that have `mod` and wolfram alpha
does.

I got the `seed` value from here: https://www.wolframalpha.com/input/?i=%287%5Ex%29+mod+20201227%3D13135480
