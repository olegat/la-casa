#!/usr/bin/env python3

import collections.abc
import enum
import random
import sys

class Pos(enum.Flag):
  START = enum.auto()
  MID   = enum.auto()
  END   = enum.auto()

class Kind(enum.Enum):
  VOWEL     = enum.auto()
  DIPHONG   = enum.auto()
  CONSONANT = enum.auto()

class Sym:
  def __init__(self, text:str, kind:Kind, position:Pos):
    self.text     = text
    self.kind     = kind
    self.position = position

  def __repr__(self):
    return f'{{"{self.text}", {self.kind}, {self.position}}}'

def symbols():
  vowl, diph, cons = (Kind.VOWEL, Kind.DIPHONG, Kind.CONSONANT)
  start, mid, end = (Pos.START, Pos.MID, Pos.END)
  anywhere = start | mid | end
  return [
    Sym(  'a', vowl, start | mid | end),
    Sym(  'e', vowl, start | mid | end),
    Sym(  'i', vowl, start | mid | end),
    Sym(  'o', vowl, start | mid | end),
    Sym(  'u', vowl, start | mid | end),
    Sym(  'y', vowl, mid | end),
    Sym( 'aa', diph, mid),
    Sym( 'ae', diph, mid),
    Sym( 'ai', diph, start | mid | end),
    Sym( 'ao', diph, mid),
    Sym( 'au', diph, start | mid),
    Sym( 'ay', diph, mid | end),
    Sym( 'ea', diph, mid | end),
    Sym( 'ee', diph, mid | end),
    Sym( 'ei', diph, start | mid | end),
    Sym( 'eo', diph, mid),
    Sym( 'eu', diph, mid),
    Sym( 'ey', diph, mid | end),
    Sym( 'er', diph, end),
    Sym('eau', diph, end),
    Sym('eei', diph, mid),
    Sym( 'ia', diph, mid | end),
    Sym( 'ie', diph, mid | end),
    #Sym( 'ii', diph, mid),
    Sym( 'io', diph, mid | end),
    Sym( 'iu', diph, mid),
    Sym( 'oa', diph, mid),
    Sym( 'oe', diph, mid),
    Sym( 'oi', diph, mid),
    Sym( 'oo', diph, mid),
    Sym( 'ou', diph, mid | end),
    Sym('ooi', diph, mid),
    Sym('oui', diph, mid),
    Sym('oei', diph, mid),
    Sym( 'ua', diph, mid),
    Sym( 'ue', diph, mid),
    Sym( 'ui', diph, mid),
    Sym( 'uo', diph, mid),
    Sym( 'uu', diph, mid),
    Sym(  "'", cons, mid), # glottal stop
    Sym(  'b', cons, start | mid | end),
    Sym(  'c', cons, start | mid | end),
    Sym(  'd', cons, start | mid | end),
    Sym(  'f', cons, start | mid | end),
    Sym(  'g', cons, start | mid | end),
    Sym(  'h', cons, start | mid | end),
    Sym(  'j', cons, start | mid),
    Sym(  'k', cons, start | mid | end),
    Sym(  'l', cons, start | mid | end),
    Sym(  'm', cons, start | mid | end),
    Sym(  'n', cons, start | mid | end),
    Sym(  'p', cons, start | mid | end),
    Sym(  'q', cons, start | mid | end),
    Sym(  'r', cons, start | mid | end),
    Sym(  's', cons, start | mid | end),
    Sym(  't', cons, start | mid | end),
    Sym(  'v', cons, start | mid | end),
    Sym(  'w', cons, start | mid | end),
    Sym(  'x', cons, start | mid | end),
    Sym(  'y', cons, start | mid | end),
    Sym(  'z', cons, start | mid | end),
    Sym( 'bl', cons, start | mid),
    Sym( 'br', cons, start | mid),
    Sym( 'ch', cons, start | mid | end),
    Sym( 'ck', cons, end),
    Sym( 'cl', cons, start | mid),
    Sym( 'cr', cons, start | mid),
    Sym( 'dr', cons, start | mid),
    Sym( 'fl', cons, start | mid),
    Sym( 'fr', cons, start | mid),
    Sym( 'fs', cons, end),
    Sym( 'gh', cons, start | mid | end),
    Sym( 'gl', cons, start | mid),
    Sym( 'gl', cons, start | mid),
    Sym( 'gn', cons, start | mid),
    Sym( 'gr', cons, start | mid),
    Sym( 'gs', cons, end),
    Sym('ght', cons, end),
    Sym( 'kh', cons, start | mid | end),
    Sym( 'kk', cons, mid | end),
    Sym( 'kl', cons, start | mid),
    Sym( 'kl', cons, start | mid),
    Sym( 'kr', cons, start | mid),
    Sym( 'ks', cons, end),
    Sym( 'kw', cons, start | mid),
    Sym( 'ld', cons, mid | end),
    Sym( 'lk', cons, mid | end),
    Sym( 'll', cons, start | mid | end),
    Sym( 'lm', cons, end),
    Sym( 'ln', cons, end),
    Sym( 'mm', cons, mid | end),
    Sym( 'nd', cons, end),
    Sym( 'ng', cons, start | mid | end),
    Sym( 'nk', cons, end),
    Sym( 'nt', cons, end),
    Sym( 'ph', cons, start | mid | end),
    Sym( 'pl', cons, start | mid),
    Sym( 'pp', cons, mid | end),
    Sym( 'pr', cons, start | mid),
    Sym( 'pt', cons, start | mid | end),
    Sym( 'qu', cons, start | mid | end),
    Sym( 'rr', cons, mid | end),
    Sym( 'rh', cons, start | mid),
    Sym( 'sh', cons, start | mid | end),
    Sym( 'sk', cons, start | mid | end),
    Sym('sch', cons, start | mid | end),
    Sym( 'th', cons, start | mid | end),
    Sym( 'tr', cons, start | mid),
    Sym( 'tw', cons, start),
    Sym( 'tt', cons, mid | end),
    Sym('thr', cons, start | mid),
    Sym( 'wk', cons, end),
    Sym( 'wl', cons, end),
    Sym( 'wm', cons, end),
    Sym( 'wn', cons, end),
    Sym('wgn', cons, end),
    Sym( 'wt', cons, end),
    Sym('wth', cons, end),
    Sym( 'zz', cons, mid),
  ]

def filter_symbols(symbols, kind=None, position=None):

  def maybe_in( value, maybe_iter ):
    if isinstance( maybe_iter, collections.abc.Iterable ):
      return value in maybe_iter;
    return False

  def fn(s):
    return \
      (kind == None or s.kind == kind or maybe_in(s.kind, kind)) and \
      (position == None or (s.position & position) == position)

  return filter( fn, symbols )

def rand_elem(ls:list):
  if isinstance(ls, filter):
    ls = list(ls)
  return ls [ random.randint(0, len(ls)-1) ]

def gen_word_symbols(symbols, minm=1, maxm=3):

  def inverse( kind ):
    if kind != Kind.CONSONANT:
      return Kind.CONSONANT
    else:
      return [Kind.VOWEL, Kind.DIPHONG]

  start = rand_elem( filter_symbols( symbols, position=Pos.START ))
  mids = [None, None]
  mids[0] = list(filter_symbols( symbols, kind=inverse(Kind.CONSONANT),
                                 position=Pos.MID ))
  mids[1] = list(filter_symbols( symbols, kind=Kind.CONSONANT,
                                 position=Pos.MID ))
  i = 0 if start.kind == Kind.CONSONANT else 1
  result = [start];
  for _ in range(random.randint( minm, maxm )):
    result.append( rand_elem(mids[i]) )
    i = (i + 1) % 2
  result.append(
    rand_elem( filter_symbols( symbols, kind=mids[i][0].kind,
                               position=Pos.END )))
  return result

def main(argv):
  syms = symbols()
  # for _ in range(100):
  #   slist = [x.text for x in gen_word_symbols( syms )]
  #   print( f'{"".join(slist)}  ({", ".join(slist)})')
  return 0

if __name__ == '__main__':
  exit(main(sys.argv))
