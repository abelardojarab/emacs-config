LanguageTool, a proof-reading tool for English, German, Polish,
Chinese, French, Dutch, Russian, Italian, Danish and more languages
(see http://www.languagetool.org/languages/ for a list)

Copyright (C) 2005-2014 the LanguageTool community and
  Daniel Naber (www.danielnaber.de)

Version 2.7, 2014-09-29
Homepage: http://www.languagetool.org

Requirements:
 -Java 7 or later (Oracle Java or IcedTea; GIJ is not supported)
 -For OpenOffice.org/LibreOffice integration: OpenOffice 3.0.1 or later
  or LibreOffice. We recommend LibreOffice 3.5.4 (or later) or
  Apache OpenOffice 3.4.1 (or later) because they contain an
  important bug fix.

Usage:
 -To integrate LanguageTool into OpenOffice.org or LibreOffice, you
  can use two methods:
 
 1. Double-click LanguageTool-2.7.oxt. The extension should
  start installing. Follow the on-screen instructions.
 
 2. If the above method doesn't work, call Tools > Extension 
  Manager > Add... in OpenOffice.org/LibreOffice and browse for the
  LanguageTool-2.7.oxt file. 
  
  Close and restart OpenOffice.org/LibreOffice Writer. Remember to close
  the QuickStarter as well if you use it. Type text with
  an error, e.g. "Feel tree to do so." - make sure the text language 
  is set to English for this example.
  You should see a blue underline under the word "tree" after about a second.
  Opening the context menu with the right mouse button on that word
  offers you a short description of the error and a correction ("free").

  If you are using LibreOffice and you want to check English or Russian texts:
  Use Options -> Language Settings -> Writing Aids -> Edit...in the
  'Tools' menu to disable LightProof and enable LanguageTool for English.
  
  Note that there will also be a new menu item "LanguageTool"
  under the "Tools" menu.
  If the native spelling and grammar dialog doesn't check grammar,
  make sure that the check box "Check Grammar" is checked in it
  (if the window closes because of no mistakes in the document,
  simply make any spelling mistake to make it open for a longer
  time, and check the box). Check also if LanguageTool is visible
  under "Grammar" in Tools > Options > Language Settings > Spelling
  for your language. Note: you can disable the grammar check without
  uninstalling LanguageTool simply by clearing the check box next to
  LanguageTool in the same dialog.
  
  Please see http://www.languagetool.org/issues/ if you
  experience problems
  
 -To use the stand-alone version, double click on the languagetool.jar file
  or call 'java -jar languagetool.jar'

 -To check plain text files from the command line, use:
  java -jar languagetool-commandline.jar <filename>

------------------------------------------------

Source code history:

On 2013-08-08 we moved our source code from Subversion at Sourceforge to
git at github (https://github.com/languagetool-org/languagetool). Most
history has been preserved. History is lost for:
 * binary files
 * branches and tags
If you need this history, the old subversion repository is still available
at http://svn.code.sf.net/p/languagetool/code/trunk/languagetool/

------------------------------------------------

License:
 
 Unless otherwise noted, this software is distributed under 
 the LGPL, see file COPYING.txt
 
 See third-party-licenses/README.txt for the copyright of the external libraries

 Language detection (*.ngp):
 The process for automatic language detection is described at
 http://languagetool.wikidot.com/adding-a-new-language-to-automatic-language-detection
 The Wikipedia-based training data can be downloaded from
 http://www.languagetool.org/download/language-training-data/
 
 Asturian:
 The Asturian data for part-of-speech tagging are from the Freeling dictionary,
 licensed under GNU General Public License.
 Contributor(s):
   Xesús González Rato <esbardu@softastur.org>

 Breton:
 The Breton data for part-of-speech tagging is based on the Apertium Breton
 dictionary under GNU General Public License with permission of its authors:
    Copyright (C) 2008--2010 Francis Tyers <ftyers@prompsit.com>
    Copyright (C) 2009--2010 Fulup Jakez <fulup.jakez@ofis-bzh.org>
    Copyright (C) 2009       Gwenvael Jekel <jequelg@yahoo.fr>
    Development supported by:
    * Prompsit Language Engineering, S. L.
    * Ofis ar Brezhoneg
    * Grup Transducens, Universitat d'Alacant

 The Breton FSA spelling dictionary is based on the Breton Hunspell dictionary
 "Difazier Hunspell an Drouizig" (0.12) licensed under the Lesser GNU Public
 License (LGPL), available at:
 http://extensions.libreoffice.org/extension-center/an-drouizig-breton-spellchecker/releases/0.12/difazier-an-drouizig-0.12

 Catalan:
 The Catalan data for part-of-speech tagging were created by Jaume Ortolà 
 based on the Freeling 3.0 and Softcatalà 2.5.0 dictionaries, both released 
 under the GNU General Public License. 
 
 Chinese:
 The Chinese data and code for part-of-speech tagging is based on ictclas4j project
 (http://code.google.com/p/ictclas4j/) under Apache License 2.0.

 Danish:
 The Danish tagger is based upon data from Stavekontrolden - Danish dictionary
 for Hunspell. © 2012 Foreningen for frit tilgængelige sprogværktøjer
 These files are published under the following open source licenses:
 GNU GPL version 2.0, GNU LGPL version 2.1, Mozilla MPL version 1.1
 http://www.stavekontrolden.dk
 Stavekontrolden is based on data from Det Danske Sprog- og Litteraturselskab
 (The Danish Society for Language and Literature), http://www.dsl.dk.

 Dutch:
 The Dutch data are partly based on Alpino parser for Dutch by Gertjan van 
 Noord and is released on LGPL license. Alpino is available at 
 http://www.let.rug.nl/~vannoord/alp/Alpino/. The POS tag system and values
 come mostly from OpenTaal, www.opentaal.org.
 
 English: see below

 French:
 The French data for part-of-speech tagging are from the Dicollecte project. 
 They are made available here under the terms of the Mozilla Public
 License 2.0 (http://mozilla.org/MPL/2.0/). See also detailed information
 in resource/fr/README_lexique.txt.

 Galician:
 The Galician data for part-of-speech tagging were created by Susana Sotelo
 Docio based on Freeling and Apertium dictionaries. Both are licensed under GPL.
 
 German:
 The German data for part-of-speech tagging is taken from Morphy
 (http://www.wolfganglezius.de/doku.php?id=cl:morphy)
 under Creative Commons Attribution-Share Alike 3.0

 Greek:
 The Greek dictionary only contains very few test entries for now,
 added by Panagiotis Minos. They are made available here under LGPL.

 Italian:
 The Italian data for part-of-speech tagging is taken from Morph-it!, 
 licensed under the Creative Commons Attribution ShareAlike 2.0 License 
 and the GNU Lesser General Public License (LGPL) 
 (see http://sslmitdev-online.sslmit.unibo.it/linguistics/morph-it.php).
 
 Khmer:
 The dictionary was obtained from these four sources:
 http://code.google.com/p/khmer-dictionary-tools/ - Part of Speech entries and words from
 * Chuon Nath's dictionary released by the Buddhist Institute of Cambodia under a BSD License
   for the LanguageTool Project
 * http://sealang.net/khmer/ - Part of Speech entries and words from Robert Headley's dictionary
   were released by Robert Headley under a BSD License for the LanguageTool Project
 * http://www.panl10n.net/english/Outputs%20Phase%202/CCs/Cambodia/MoEYS/Software/2009/KhmerCorpus.zip
   Released under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License
   for the LanguageTool Project
 * http://www.sbbic.org - The Society for Better Books of Cambodia has made changes to all 
   these sources, correcting, and adding new words in order to improve the grammar checker -
   SBBIC releases these changes and additions under a BSD License for the LanguageTool Project

 Malayalam:
 The data has been collected by Jithesh.V.S. of the Centre For Development of Imaging Technology (C-DIT),
 Thiruvananthapuram, Kerala, India (http://www.cdit.org). It comes from public sources like newspapers, 
 magazines, and novels. It is made available here under GPL. 
 
 Polish:
 The Polish data for part-of-speech tagging is from the Morfologik project,
 licensed on BSD (see http://morfologik.blogspot.com).

 Romanian:
 The Romanian data for part-of-speech tagging is developed by Ionuț Păduraru
 (http://www.archeus.ro). It's being released here on LGPL license.

 Russian:
 Russian dictionary originally developed by www.aot.ru and licensed under LGPL.
 http://www.aot.ru/download.php file rus-src-morph.tar.gz
 or http://sourceforge.net/p/seman/svn/HEAD/tree/trunk/Dicts/SrcMorph/RusSrc/    file  morphs.mrd
 It was partially converted to fsa format in 2008-2011, 2014 by Yakov Reztsov.  
 Frequency information for spell-checking dictionary from www.aot.ru
 Source frequency information http://sourceforge.net/p/seman/svn/HEAD/tree/trunk/Dicts/SrcBinDict/  file   WordData.txt
 It was converted to use with spell-checking dictionary in 2014 by Yakov Reztsov.  

 Slovak:
 The Slovak data were created by Zdenko Podobný based on Slovak National
 Corpus data (http://korpus.juls.savba.sk/). They are released here on
 LGPL license.

 Spanish:
 The dictionary was mainly obtained from the Freeling project.
   http://devel.cpl.upc.edu/freeling/svn/latest/freeling/data/es/dicc.src
   http://garraf.epsevg.upc.es/freeling/
 It is released under the GNU General Public License.

 Swedish:
 The Swedish data are based on DSSO. The Initial Developer of the Original Code is Göran Andersson.
 Contributor(s):
   Tom Westerberg <tweg@welho.com>
   Niklas Johansson <sleeping.pillow@gmail.com>
 The Swedish Dictionary may be used under the terms of the GNU Lesser General Public License Version 2.1 or later 
 (the "LGPL"). 
 http://dsso.se
 
 Tagalog:
 The Tagalog Tagset was designed by Nathaniel Oco.
 The words for the Tagger Dictionary were taken from the Philippine Literature Domain of Dalos D. Miguel's Comparative Analysis of Tagalog POS Taggers.
 The Tagger Dictionary and the Tagset are made available under LGPL.
 The Trigram Training Data is available at: The Trigram Training Data is available at: https://sourceforge.net/projects/tattoi.u/files/Trigram%20Text/

 Tamil:
 The Tamil dictionary, tagset and rules were created by
 Ve. Elanjelian <tamiliam@gmail.com>. It is released under GPLv3 licence.
  
 *	The work owes much to his previous work with Hunspell spellchecker, a project
	that has had many contributors including S. Muguntharaj, Radhakrishnan, Vijay,
	A. Suji, Malathi Selvaraj, Sri Ramadoss, Yagna Kalyanaraman, and Pranava Swaroop.
 *	The work also made use the Tamil corpus created by Crubadan 2.0
	<http://borel.slu.edu/crubadan/stadas.html> for shortlisting nouns and verbs.
	The corpus data is released under GPLv3, as well.
 *	The grammatical rules themselves are based on "thamizhnadaik kaiyEdu" (2004)
	and "thamizhil nAmum thavaRillAmal ezhuthalAm" (2007)
 
 Ukrainian:
 The Ukrainian data for part-of-speech tagging were created by Andriy Rysin
 It's based on spell-uk (http://sourceforge.net/projects/ispell-uk/) which is licensed under GPL/LGPL.

------------------------------------------------ 
 
 English:
 The English data for part-of-speech tagging are based on:
 
 1) Automatically Generated Inflection Database (AGID) version 4, 
 Copyright 2000-2003 by Kevin Atkinson <kevina@gnu.org>
 The part-of-speech database is taken from Alan Beale 2of12id 
 and the WordNet database which is under the following copyright:

    This software and database is being provided to you, the LICENSEE, by
    Princeton University under the following license.  By obtaining, using  
    and/or copying this software and database, you agree that you have  
    read, understood, and will comply with these terms and conditions.:  
  
    Permission to use, copy, modify and distribute this software and
    database and its documentation for any purpose and without fee or
    royalty is hereby granted, provided that you agree to comply with  
    the following copyright notice and statements, including the disclaimer,  
    and that the same appear on ALL copies of the software, database and  
    documentation, including modifications that you make for internal  
    use or for distribution.  
  
    WordNet 1.6 Copyright 1997 by Princeton University.  All rights reserved.  
  
    THIS SOFTWARE AND DATABASE IS PROVIDED "AS IS" AND PRINCETON  
    UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES, EXPRESS OR  
    IMPLIED.  BY WAY OF EXAMPLE, BUT NOT LIMITATION, PRINCETON  
    UNIVERSITY MAKES NO REPRESENTATIONS OR WARRANTIES OF MERCHANT-  
    ABILITY OR FITNESS FOR ANY PARTICULAR PURPOSE OR THAT THE USE  
    OF THE LICENSED SOFTWARE, DATABASE OR DOCUMENTATION WILL NOT  
    INFRINGE ANY THIRD PARTY PATENTS, COPYRIGHTS, TRADEMARKS OR  
    OTHER RIGHTS.
  
    The name of Princeton University or Princeton may not be used in  
    advertising or publicity pertaining to distribution of the software  
    and/or database.  Title to copyright in this software, database and  
    any associated documentation shall at all times remain with  
    Princeton University and LICENSEE agrees to preserve same.  

  Alan Beale 2of12id.txt is indirectly derived from the Moby part-of-speech
  database and the WordNet database.  The Moby part-of-speech is in the
  public domain:

    The Moby lexicon project is complete and has
    been place into the public domain. Use, sell,
    rework, excerpt and use in any way on any platform.
    
    Placing this material on internal or public servers is
    also encouraged. The compiler is not aware of any
    export restrictions so freely distribute world-wide.
    
    You can verify the public domain status by contacting
    
    Grady Ward
    3449 Martha Ct.
    Arcata, CA  95521-4884
    
    grady@netcom.com
    grady@northcoast.com

    For more information on wordlists used, see agid-readme.txt.

  2) Part Of Speech Database, compiled by Kevin Atkinson 
  <kevina@users.sourceforge.net>
   The part-of-speech.txt file contains is a combination of 
   "Moby (tm) Part-of-Speech II" and the WordNet database (see above and 
   pos-readme.txt).
   
  3) 2of12inf wordlist, released to public domain, 
  see 12dicts-readme.html.
  
  4) Public domain Moby wordlists were used also for generating 
  POS tag information for common proper names.
  
  For more information, see the scripts in the source directory 
  en/resource/.  
