#!/usr/bin/env python
# coding: utf8
import nltk

nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('maxent_ne_chunker')
nltk.download('words')

from nltk import word_tokenize, pos_tag, ne_chunk


TEXTS = """
        A few weeks ago my grandfather was diagnosed with cancer.

It took me by surprise, as it would take most people I guess, but it got my head spinning around an idea: what if I don't have the time I always thought I'd have?

That one was a shocker. It kept me in bed for a few days. It was one of those moments where you change an a-priori conception, and life as you know it ends. Everything changes, as per a sadistic magician's ultimate trick, and you're left with just one thing: yourself. Damn you Kant.

Nurture yourself, now. Because the problem is not thinking we barely have any time to dedicate to ourselves and put our sweat and blood into what makes us actually happy because of our tight schedules, and busy work and social lives or whatnot; the problem is that we are right about it –I mean, the part about not having time.

We really don't, you can't own time. It's said around the interwebs that if you reach 30 and are still a virgin, you become a Wizard with the powers of all nyan cats combined with the rep of all redditors, thus allegedly being able to store time in magical tarballs and rewrite it at will. For the rest of us mortals, it passes by with every second and it's unrecoverable. Gonzo. Au revoir. Chau. Nothing can be done about it. There's no git rebase -i , no Ctrl+Z, no rewind button. You're screwed.

And with that time constraint, came the obvious question of whether all this little precious moments we call Present that I dedicated to engineering, had had any fruitful results after all.

I asked myself, are you comfortable with what you've accomplished so far? And the answer was almost always negative.

After it stayed like that for a couple weeks, I began to evaluate my career as a software engineer. And by Odin's Beard, something pulled me towards carpentry.

So I stopped programming
I designed some furniture like a good programmer should. Modular, self-documenting, functional. You'd see it, realize it was a bed made of pallets, could rearrange it, and either choose for a leg-side or a head-side omni-nightstand just by moving the mattress across the longest horizontal axis. Pretty cool, I thought.

As I started working on it I began to feel that strange sensation you get when doing something that in your head seemed so easy and quick to do, and in reality was so time consuming and error prone. I mean, how long can it take to make a wooden bed? How hard can it be?

The pallets were dirty, had plenty of imperfections (mostly some eyes in the wood), and it took me about 7 hours to completely hand-sand the first one until I was –give or take– okay with the result. I became slightly more wood-wise. I painted it, and it was fun and rather quickly done.

This whole process can be quite cogitative. It can really get through you. And when it did get through me, I started asking myself all kinds of questions and, well, you kinda try to heuristically determine which are the important ones and which paths are dead-ends, to see if you can get somewhere.

Was what I was doing any good? To which degree had my craft any quality?

I now see I was actually talking about engineering, but at that time it seemed like I was having quite an inner dialogue about how good bad a carpenter I was.

Man, I was so disappointed. I seriously began to doubt my skills, yet continued working on the bed in hopes it would turn out okay and, eventually, it was finished.

It did take far more than I thought, in terms of patience and dedication. But not a moment passed after I dropped myself on that bed, that I realized how much I had learned about crafting in general that also applied to my body of work as an engineer! I was astounded!

Memories came back, of that project I made once just to prove myself I could do it, and that small contribution to a popular open source project, and to unpopular ones too. My head was again filled with ideas, code, patterns and anti-patterns, and shell scripts, and repositories, and architectures...you know how it goes.

Realizing that it really is about nurturing your craft to the point where you are satisfied with it, and about the patience to go through every itsy bitsy detail with the love that a grandfather puts in teaching his 10 year's old grandson how to swim, how to keep breathing, how not to drown.

It's that process the one that takes the most time. <span class="key">The process of nurturing. In fact, I'm not certain it takes a discrete amount of time, it might take forever (which of course, we don't have), but it's perhaps the act of dedicating the most of all these precious little moments to nurture ourselves what makes it possible to really achieve a higher degree of quality in anything we do.

Now the question stops being "will I have time to be better at something?", and becomes "will I dedicate this moment to be better at something?". That's a radical change of perspective, but I have a feeling it's way more spot on.

Time will tell.

As for my grandfather, I'm trying to visit him as often as I can now that he's back home. I'll be having him join my impromptu carpentry adventures immediately. He's even got some mad wood-work skills.

As for me, today I started reading about Riak and Erlang again. I'm an engineer at heart, little can be done against that I guess. But despite having a few more pieces of furniture that I need to get done in the next few weeks, something tells me it won't be long till I see vim again.
        """.split("\n")


if __name__ == "__main__":
    for line in TEXTS:
        print(ne_chunk(pos_tag(word_tokenize(line))))
