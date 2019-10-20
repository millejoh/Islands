import gui.string
import tcod

t1 = "Lisp is the {red}red{/} pill."
t2 = "G{'i}mli, son of Glo{'i}n."
t3 = "{fg:blue}Python{/} is also quite {bg:green}nice{/}."
t4 = "A test setting by foreground and background {fg:cyan,bg:grey}color{/}."
t5 = "{click:LABEL}Click me.{/}"

def test_tokens():
    toks = [val for val in gui.string.next_token(t4, gui.string.master_pat)]
    asset len(toks) == 5
    assert toks[0] = gui.string.Token(type='TEXT', value='A test setting by foreground and background')

def test_foreground_color():
    assert '{:c}'.format(tcod.COLCTRL_FORE_RGB) == '\x06'

def test_color_to_control_string():
    c = gui.string.color_to_control_string(255)
    assert c == '\x06ÿ\x01\x01'

def test_make_colored_string():
    s = gui.string.make_colored_string(t3)
    assert t3 == '\x06\x01\x01ÿPython\x08 is also quite \x07\x01\x80\x01nice\x08.'
