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
  CONSONANT = enum.auto()


class Sym:
  def __init__(self, text:str, kind:Kind, position:Pos):
    self.text     = text
    self.kind     = kind
    self.position = position

  def __repr__(self):
    return f'{{"{self.text}", {self.kind}, {self.position}}}'


class SymList:
  def __init__(self, syms=None):
    if syms == None:
      syms = default_symbolist()
    self._iter = syms;

  def _maybe_expand(self):
    if isinstance(self._iter, filter):
      self._iter = list(self._iter)

  def __repr__(self):
    return str(self._iter)

  def __len__(self):
    self._maybe_expand()
    return len(self._iter)

  def __getitem__(self, key):
    self._maybe_expand()
    return self._iter.__getitem__(key)

  def __setitem__(self, key, value):
    self._maybe_expand()
    return self._iter.__setitem__(key, value)

  def filter(self, kind=None, position=None):

    def maybe_in( value, maybe_iter ):
      if isinstance( maybe_iter, collections.abc.Iterable ):
        return value in maybe_iter;
      return False

    def fn(sym):
      return \
        (kind == None or sym.kind == kind or maybe_in(sym.kind, kind)) and \
        (position == None or (sym.position & position) == position)

    return SymList(filter( fn, self._iter ))

  def randelem(self):
    i = random.randint(0, len(self)-1)
    return self._iter[i]


class WordGenerator:
  def __init__(self, symlist:SymList=None, minm=1, maxm=2):
    if symlist == None:
      symlist = SymList()
    self.symlist = symlist
    self.minm = minm
    self.maxm = maxm
    # Precompute filtered lists:
    self._mids = [None, None]
    self._ends = [None, None]
    self._starts = symlist.filter(position=Pos.START)
    self._mids[0] = symlist.filter(kind=Kind.VOWEL,     position=Pos.MID)
    self._mids[1] = symlist.filter(kind=Kind.CONSONANT, position=Pos.MID)
    self._ends[0] = symlist.filter(kind=Kind.VOWEL,     position=Pos.END)
    self._ends[1] = symlist.filter(kind=Kind.CONSONANT, position=Pos.END)

  def gen_symbols_sequence(self):
    result = [self._starts.randelem()]
    i = 0 if result[0].kind == Kind.CONSONANT else 1
    for _ in range( random.randint(self.minm, self.maxm) ):
      result.append( self._mids[i].randelem() )
      i = (i + 1) % 2
    result.append(self._ends[i].randelem())
    return result


# Many backend require at least one number and one capital letter.
# This special generator generates exactly of each.
class TagGenerator:
  def __init__(self):

    def _syms(texts:list, kind:Kind):
      return [Sym(str(text), kind, Pos.START|Pos.END) for text in texts]

    # Exclude [1, I] and [0, O] which are easily confused.
    nums  = _syms([2, 3, 4, 5, 6, 7, 8, 9], Kind.VOWEL)
    chars = _syms(list('ABCDEFGHJKLMNPQRSTUVWXYZ'), Kind.CONSONANT)
    self._wg = WordGenerator(SymList(nums+chars), minm=0, maxm=0)

  def gen_symbols_sequence(self):
    return self._wg.gen_symbols_sequence();


class PasswordGenerator:
  def __init__(self, symlist:SymList=None):
    self._wg = WordGenerator(symlist)
    self._tg = TagGenerator();

  def gen_password(self):
    gs = [self._wg] * 3
    gs.insert( random.randint(0,2), self._tg )
    words = [''.join([s.text for s in g.gen_symbols_sequence()]) for g in gs]
    return '-'.join(words)


def default_symbolist():
  return englishy_symbolist();

def englishy_symbolist():
  vowl, cons = (Kind.VOWEL, Kind.CONSONANT)
  start, mid, end = (Pos.START, Pos.MID, Pos.END)
  anywhere = start | mid | end
  return [
    # Basic vowel
    Sym('a'     , vowl, mid | end),
    Sym('e'     , vowl, mid | end),
    Sym('i'     , vowl, mid | end),
    Sym('o'     , vowl, mid | end),
    Sym('u'     , vowl, mid | end),
    Sym('ai'    , vowl, mid | end),
    Sym('au'    , vowl, mid),
    Sym('ea'    , vowl, mid | end),
    Sym('ee'    , vowl, mid | end),
    Sym('ei'    , vowl, mid | end),
    Sym('eu'    , vowl, mid),
    Sym('ie'    , vowl, mid | end),
    Sym('io'    , vowl, mid | end),
    Sym('iu'    , vowl, mid),
    Sym('oa'    , vowl, mid),
    Sym('oe'    , vowl, mid),
    Sym('oi'    , vowl, mid),
    Sym('oo'    , vowl, mid | end),
    Sym('ou'    , vowl, mid | end),
    Sym('ui'    , vowl, mid),

    # Basic consonants
    Sym('b'     , cons, mid),
    Sym('c'     , cons, mid | end),
    Sym('d'     , cons, mid | end),
    Sym('f'     , cons, mid | end),
    Sym('g'     , cons, mid | end),
    Sym('h'     , cons, mid | end),
    Sym('j'     , cons, start),
    Sym('k'     , cons, mid | end),
    Sym('l'     , cons, mid | end),
    Sym('m'     , cons, mid | end),
    Sym('n'     , cons, mid | end),
    Sym('p'     , cons, mid | end),
    Sym('q'     , cons, end),
    Sym('r'     , cons, mid | end),
    Sym('s'     , cons, mid | end),
    Sym('t'     , cons, mid | end),
    Sym('v'     , cons, mid),
    Sym('w'     , cons, mid | end),
    Sym('x'     , cons, end),
    Sym('y'     , cons, mid),
    #Sym('z'     , cons, mid | end),

    # Other consonants
    Sym('bl'    , cons, mid),
    Sym('bl'    , cons, start | mid),
    Sym('br'    , cons, start | mid),
    Sym('ch'    , cons, start | mid | end),
    Sym('cl'    , cons, start | mid),
    Sym('cr'    , cons, start | mid),
    Sym('dr'    , cons, start | mid),
    Sym('fl'    , cons, start | mid),
    Sym('fr'    , cons, start | mid),
    Sym('gh'    , cons, start | mid | end),
    Sym('gl'    , cons, start | mid),
    Sym('gn'    , cons, start | mid),
    Sym('gr'    , cons, start | mid),
    Sym('kk'    , cons, mid),
    Sym('ll'    , cons, mid),
    Sym('mm'    , cons, mid),
    Sym('nn'    , cons, mid),
    Sym('ph'    , cons, start | mid | end),
    Sym('pl'    , cons, start | mid),
    Sym('pr'    , cons, start | mid),
    Sym('rh'    , cons, start),
    Sym('sh'    , cons, start | mid | end),
    Sym('sk'    , cons, start | mid | end),
    Sym('sch'   , cons, start | mid | end),
    Sym('th'    , cons, start | mid | end),
    Sym('tr'    , cons, start),
    Sym('tw'    , cons, start),
    Sym('tt'    , cons, mid | end),

    # Common word prefixes (ending in vowels):
    Sym('de'    , vowl, start),
    Sym('deo'   , vowl, start),
    Sym('edo'   , vowl, start),
    Sym('entro' , vowl, start),
    Sym('extra' , vowl, start),
    Sym('geo'   , vowl, start),
    Sym('giga'  , vowl, start),
    Sym('hydro' , vowl, start),
    Sym('insta' , vowl, start),
    Sym('kilo'  , vowl, start),
    Sym('maxi'  , vowl, start),
    Sym('mega'  , vowl, start),
    Sym('micro' , vowl, start),
    Sym('milli' , vowl, start),
    Sym('mini'  , vowl, start),
    Sym('multi' , vowl, start),
    Sym('nano'  , vowl, start),
    Sym('necro' , vowl, start),
    Sym('nylo'  , vowl, start),
    Sym('nympo' , vowl, start),
    Sym('phy'   , vowl, start),
    Sym('psy'   , vowl, start),
    Sym('pyro'  , vowl, start),
    Sym('qua'   , vowl, start),
    Sym('qua'   , vowl, start),
    Sym('que'   , vowl, start),
    Sym('qui'   , vowl, start),
    Sym('quo'   , vowl, start),
    Sym('re'    , vowl, start),
    Sym('rea'   , vowl, start),
    Sym('tera'  , vowl, start),
    Sym('ultra' , vowl, start),
    Sym('uni'   , vowl, start),
    # Common word prefixes (ending in consonants):
    Sym('add'   , cons, start),
    Sym('adr'   , cons, start),
    Sym('air'   , cons, start),
    Sym('amp'   , cons, start),
    Sym('app'   , cons, start),
    Sym('appl'  , cons, start),
    Sym('as'    , cons, start),
    Sym('ass'   , cons, start),
    Sym('assim' , cons, start),
    Sym('ath'   , cons, start),
    Sym('athl'  , cons, start),
    Sym('attr'  , cons, start),
    Sym('ed'    , cons, start),
    Sym('egg'   , cons, start),
    Sym('eigh'  , cons, start),
    Sym('eph'   , cons, start),
    Sym('ess'   , cons, start),
    Sym('eth'   , cons, start),
    Sym('ex'    , cons, start),
    Sym('exc'   , cons, start),
    Sym('excr'  , cons, start),
    Sym('ill'   , cons, start),
    Sym('imm'   , cons, start),
    Sym('in'    , cons, start),
    Sym('inn'   , cons, start),
    Sym('ins'   , cons, start),
    Sym('instit', cons, start),
    Sym('int'   , cons, start),
    Sym('inter' , cons, start),
    Sym('irr'   , cons, start),
    Sym('thr'   , cons, start),
    Sym('un'    , cons, start),

    # Common word suffixes (starting with a vowel)
    Sym('all'   , vowl, end),
    Sym('alls'  , vowl, end),
    Sym('alm'   , vowl, end),
    Sym('alms'  , vowl, end),
    Sym('aln'   , vowl, end),
    Sym('alns'  , vowl, end),
    Sym('amp'   , vowl, end),
    Sym('amps'  , vowl, end),
    Sym('arm'   , vowl, end),
    Sym('arms'  , vowl, end),
    Sym('arn'   , vowl, end),
    Sym('arns'  , vowl, end),
    Sym('aw'    , vowl, end),
    Sym('awl'   , vowl, end),
    Sym('awls'  , vowl, end),
    Sym('awn'   , vowl, end),
    Sym('awns'  , vowl, end),
    Sym('aws'   , vowl, end),
    Sym('ay'    , vowl, end),
    Sym('eau'   , vowl, end),
    Sym('ell'   , vowl, end),
    Sym('ells'  , vowl, end),
    Sym('elm'   , vowl, end),
    Sym('elms'  , vowl, end),
    Sym('eo'    , vowl, end),
    Sym('er'    , vowl, end),
    Sym('erl'   , vowl, end),
    Sym('erls'  , vowl, end),
    Sym('erm'   , vowl, end),
    Sym('erms'  , vowl, end),
    Sym('ern'   , vowl, end),
    Sym('erns'  , vowl, end),
    Sym('ers'   , vowl, end),
    Sym('es'    , vowl, end),
    Sym('ey'    , vowl, end),
    Sym('ia'    , vowl, end),
    Sym('ial'   , vowl, end),
    Sym('ials'  , vowl, end),
    Sym('ies'   , vowl, end),
    Sym('ill'   , vowl, end),
    Sym('ills'  , vowl, end),
    Sym('imp'   , vowl, end),
    Sym('imps'  , vowl, end),
    Sym('ing'   , vowl, end),
    Sym('ings'  , vowl, end),
    Sym('ins'   , vowl, end),
    Sym('ion'   , vowl, end),
    Sym('ions'  , vowl, end),
    Sym('irn'   , vowl, end),
    Sym('irns'  , vowl, end),
    Sym('irt'   , vowl, end),
    Sym('irts'  , vowl, end),
    Sym('oll'   , vowl, end),
    Sym('olls'  , vowl, end),
    Sym('omp'   , vowl, end),
    Sym('omps'  , vowl, end),
    Sym('orm'   , vowl, end),
    Sym('orms'  , vowl, end),
    Sym('orn'   , vowl, end),
    Sym('orns'  , vowl, end),
    Sym('ort'   , vowl, end),
    Sym('orts'  , vowl, end),
    Sym('ow'    , vowl, end),
    Sym('owl'   , vowl, end),
    Sym('owls'  , vowl, end),
    Sym('ows'   , vowl, end),
    Sym('ull'   , vowl, end),
    Sym('ulls'  , vowl, end),
    Sym('ump'   , vowl, end),
    Sym('umps'  , vowl, end),
    Sym('urm'   , vowl, end),
    Sym('urms'  , vowl, end),
    Sym('urn'   , vowl, end),
    Sym('urns'  , vowl, end),
    Sym('urt'   , vowl, end),
    Sym('urts'  , vowl, end),
    Sym('y'     , vowl, end),
    # Common word suffixes (starting with a consonants):
    Sym('ck'    , cons, end),
    Sym('cks'   , cons, end),
    Sym('ffirm' , cons, end),
    Sym('ffirms', cons, end),
    Sym('firm'  , cons, end),
    Sym('firms' , cons, end),
    Sym('fs'    , cons, end),
    Sym('gs'    , cons, end),
    Sym('ght'   , cons, end),
    Sym('ghts'  , cons, end),
    Sym('ks'    , cons, end),
    Sym('ld'    , cons, end),
    Sym('lds'   , cons, end),
    Sym('lk'    , cons, end),
    Sym('lks'   , cons, end),
    Sym('ls'    , cons, end),
    Sym('nd'    , cons, end),
    Sym('nds'   , cons, end),
    Sym('ng'    , cons, end),
    Sym('ngs'   , cons, end),
    Sym('nk'    , cons, end),
    Sym('nks'   , cons, end),
    Sym('nt'    , cons, end),
    Sym('nts'   , cons, end),
    Sym('ny'    , cons, end),
    Sym('nys'   , cons, end),
    Sym('pt'    , cons, end),
    Sym('ject'  , cons, end),
    Sym('jects' , cons, end),
    Sym('jer'   , cons, end),
    Sym('jers'  , cons, end),
    Sym('jet'   , cons, end),
    Sym('jets'  , cons, end),
    Sym('joist' , cons, end),
    Sym('pts'   , cons, end),
    Sym('rs'    , cons, end),
    Sym('se'    , cons, end),
    Sym('ses'   , cons, end),
    Sym('sks'   , cons, end),
    Sym('ths'   , cons, end),
    Sym('thes'  , cons, end),
    Sym('ve'    , cons, end),
    Sym('ves'   , cons, end),
  ]

def main(argv):
  # pwg = PasswordGenerator()
  # for _ in range(100):
  #   print(pwg.gen_password())
  wg = WordGenerator()
  for _ in range(1000):
    slist = [x.text for x in wg.gen_symbols_sequence()]
    print( f'{"".join(slist)}  ({", ".join(slist)})')
  return 0

if __name__ == '__main__':
  exit(main(sys.argv))
