{
    This file is part of the Free Pascal/NewPascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the NewPascal development team (http://newpascal.org)

    Copyright(c) 2004-2018 DaThoX

    It contains the generics collections library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 !!! IMPORTANT NOTE about usage of Generics.Collections and bug reports !!!

 author of this library has no access to FPC trunk anymore, so every problem
 related to this library should be reported here :

 https://github.com/maciej-izak/generics.collections/issues

 The library is compatible with NewPascal, FPC 3.0.4 and FPC trunk, every patch
 (if possible) will be re-reported to FPC bugtracker with proper patch by main author.
 Compatibility with FPC 3.0.4 and trunk will be provided as long as possible.

 The NewPascal has special support for this library, more recent version (more 
 bug fixes), more optimizations and better support from compiler side 
 (NewPascal contains modified/extended FPC compiler version).

 **********************************************************************}

unit Generics.Strings;

{$mode objfpc}{$H+}

interface

resourcestring
  SArgumentOutOfRange = 'Argument out of range';
  SArgumentNilNode = 'Node is nil';
  SDuplicatesNotAllowed = 'Duplicates not allowed in dictionary';
  SCollectionInconsistency = 'Collection inconsistency';
  SCollectionDuplicate = 'Collection does not allow duplicates';
  SDictionaryKeyDoesNotExist = 'Dictionary key does not exist';
  SItemNotFound = 'Item not found';

implementation

end.

