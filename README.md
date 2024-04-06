# Finite-State-Transducers-and-Phonological-Rules
 I used a Finite State Transducer (FST) to observe how phonological rules can be ordered in order to create the right conditions to achieve Canadian Vowel Raising. Canadian Vowel Raising is a phenomenon where vowels are pronounced higher when followed by a voiceless consonant like /p/, /t/, /k/, /s/, and /f/. North American English also utilizes the voiced alveolar tap /ɾ/ when followed and preceded by a vowel. In this project, I created an FST that performed vowel raising on the diphthong /ɑɪ/ and raise it to become /ʌɪ/ when followed by unvoiced consonant. In phonology, this rule would be represented as: 
		
    ɑɪ →  ʌɪ / __ [ - voiced]
		
    ɑɪ →  ɑɪ / __ [ + voiced]

We will call this Rule A.

Another phonological rule that I encoded into an FST is changing the intervocalic alveolar stop into a tap:
		
    t → ɾ / V __ V
		
    d → ɾ /  V __ V

We will call that one Rule B.

Firstly, in order to define my alphabet in the FST, I used lowercase characters from ‘a’ to ‘z’. In order to represent the diphthongs, I used ‘A’ to represent /ɑɪ/ and ‘O’ to represent /ʌɪ/. For the tap /ɾ/, I represented it using the character ‘T’.

*A caveat to note is that the alphabet in this project is a little odd as it uses both IPA phonemes and English characters. The reason that I was not able to do everything in IPA as I had wished was because there were many phonemes that I did not know how to represent as a single character in a way that would be meaningful for the reader to understand. For example, phonemes like /ʃ/, /ŋ/ are hard to represent with just one meaningful character and assigning it a random character would make the actual surface level and underlying representations hard to decipher. Also, other characters in IPA such as /j/ have a different pronunciation from the English letter so for the sake of simplicity and clarity, I chose to represent all characters in my alphabet as what they are intended to sound like in the english alphabet aside for ‘A’, ‘O’, and ‘T’ which represent their respective diphthongs and taps.

Seeing it in GHCI:
To view how the code works in representing the phonological rules, all the haskell files should be downloaded and it should be loaded into ghci by typing: :load FinalProj.hs

After all the modules have been loaded, fstVowelRaising can be used with the transduce function in order to see how different types of strings will be transduced using this specific FST. We can transduce the string “hike”, which will be represented as /hAke/ in haskell because of the use of the ɑɪ diphthong. To do this, we can input into ghci:

    transduce fstVowelRaising "hAke"

Here, we see that because the unraised vowel preceded a voiceless consonant ‘k’, the transducer applied the phonological rule, thus raising the vowel and outputting: 
    
    ["hOke"]
