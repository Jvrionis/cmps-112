#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"===================================================================="
"    James Vrionis                                                  *"
"    Luck Tanner                                                    *"
"    W.Mackey                                                       *"
"    CMPS112-wm                                                     *"
"    ASG3                                                           *"
"===================================================================="


"===================================================================="
" charfreq.st "
"===================================================================="
nl := Character nl.
Character extend [
   isPrint [ ^ (Character space <= self) & (self <= $~)
   ]
   singleDigit [ ^ (self asInteger) > 15
   ]
   visible [ self isPrint ifTrue: [^' ', self asString ]
      ifFalse: [ self singleDigit   
         ifTrue: [ ((self asInteger) = 256)
            ifTrue: [ ^ 'EOF ' ] 
            ifFalse:[^ 'x', ((self asInteger radix: 16) copyFrom: 4) ]
         ]
         ifFalse: [^ 'x0', ((self asInteger radix: 16) copyFrom: 4) ]
      ]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]
   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]
   init: size [
      theArray := Array new: size.
   ]
   size [
      ^ theArray size.
   ]
   at: index [
      ^ theArray at: index + 1.
   ]
   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]
   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0 ].
      self at: index put: (self at: index) + 1.
   ]
   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]
"===================================================================="

"===================================================================="
" tree.st "
"===================================================================="
Object subclass: Leaf [
   |char count isleaf|
   char [ ^ char ]
   count [ ^ count ]
   isleaf [ ^ isleaf ]
   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
      isleaf := true. " is an external node "
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   " Need a post-order traversal "
   postorder: visitor [
      visitor value: char value: isleaf.
   ]
]

" Tree subclass of leaf modified from tree.st "
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount 
                              left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
      isleaf := false. 
   ]

   right [ ^ right ]
   left [ ^ left ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left  inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor [
      left postorder: visitor. "left first"
      right postorder: visitor. "right next"
      visitor value: char value: isleaf.
   ]
]
"===================================================================="

"===================================================================="
" Outbits.st "
"===================================================================="
Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]
"===================================================================="

"===================================================================="
" bitstream.st "
"===================================================================="
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]
"===================================================================="

|inputfile readfile outputfile dcpfile outfilename |

" Initialize dtcu options to false "
" Priority Queue: used for building tree "
Dopt := Topt := Copt := Uopt := false.
outfilename := stdout.
priorityqueue := SortedCollection new.

" NRC = Necessary Run Condition "
" NRC: 1 < Command line arguments < 4 "
(Smalltalk arguments size > 1) & (Smalltalk arguments size < 4)
ifTrue: [ 
   " Parse command line arguments "
   (1 to: Smalltalk arguments size) do: [:index |
         argv := (Smalltalk arguments at: index).
      " NRC: Check for '-' prefixed to d,t,c or d,t,u "
      (argv includes: $-) ifTrue: [    
         (argv includes: $d) ifTrue: [ Dopt := true. ].
         (argv includes: $t) ifTrue: [ Topt := true. ].
         (argv includes: $c) ifTrue: [ Copt := true. ].
         (argv includes: $u) ifTrue: [ Uopt := true. ].    
      ]

      " NRC: does an 'infile' exist "
      ifFalse: [ (infile ~= nil) 
         ifFalse: [ infile := argv.]
         " NRC: if 'infile' exist...does a'outfile' exist "
         ifTrue: [ outfilename := argv. ]
      ]
   ]. " end of do: "
]
" NRC: Inproper execution error with NRC's "
ifFalse: [
   stderr << 'Error (1): Usage: hzip.st -dtcu inputfile [outputfile]'
                                                               << nl.
   ObjectMemory quit: 1
].

" Invalid combination of options '-cu' exception handling "
(Copt & Uopt) ifTrue: [
   stderr << 'Invalid Option Error (2): , -c must not be used with -u!'
                                                               << nl.
   ObjectMemory quit: 1
].

" Print Debug information per application author convenience "
Dopt ifTrue: [
   stdout << 'Dubbing Invoked -d: on input file ' << infile << ' || '
   << ' to output file ' << outfilename << nl.
].

" Check for existence of inputfile specified in args "
(File exists: infile) ifFalse: [ 
   stderr << 'Input File Error(3): file ' << infile << 
                                           ' does not exist.' << nl.
   ObjectMemory quit: 1 
].

|inputfile writefile|
inputfile := FileStream open: infile mode: FileStream read.

outfilename = stdout
ifTrue: [ 
   writefile := nil. 
]
ifFalse: [
   writefile := FileStream open: outfilename mode: FileStream write.
].
" Frequency Table: insert EOF "
freqtable := ZeroArray new: 257.
freqtable at:256 put:1.
"==================================================================="

"==================================================================="
" if NOT Option -u "
"==================================================================="
Uopt ifFalse: [
  "encode: Table to hold huffman encoding for chars "
   encode := ZeroArray new: 257.
   
   [inputfile atEnd not] whileTrue: [
      |ordChar|
      ordChar := inputfile next asInteger.
      freqtable incr: ordChar.
   ].

   "Populate Priority queue with all Frequency Table elements "
   freqtable keysAndValuesDo: [:key :value |
      (value notNil and: [value > 0]) ifTrue: [
         priorityqueue add: (Leaf new: key count: value).
      ]
   ].

   " Construct Huffman Tree: "
   " Add Frequency Table elements into the Priority queue "
   " left = left child, right = right child "
   " tcnt = table counter, tchar = table char "
   [priorityqueue size > 1] whileTrue: [
      |left right tcnt tchar|
      left    := priorityqueue removeFirst.
      right   := priorityqueue removeFirst.
      tchar   := left char.
      tcnt    := (right count + left count).
      newTree := Tree new: tchar count: tcnt left: left right: right.
      priorityqueue add: newTree.
   ].

   " Build Encoding table via In-order traversal "
   root := priorityqueue removeFirst.
   (root notNil ) ifTrue: [
      root inorder: [:char :string |
         encode at: char put: string.
      ] prefix: ''.
   ].

"===================================================================="

"===================================================================="
" OPTION -t: Display encoding table "
"===================================================================="
   Topt ifTrue: [
     |count tmp  val char bitset |
     count := 0.
     
      encode keysAndValuesDo: [:key :val |
         (val notNil and: [val > 0]) ifTrue: [

            char := key asCharacter visible.
            bitset := encode at: key.
            stdout << key asCharacter visible.
            
            char asString size to: 5 do: [:skip| stdout << ' ' ].
            stdout << (freqtable at: key).
            tmp := (freqtable at: key). 
            tmp asString size to: 8 do: [:skip| stdout << ' ' ].
            stdout << bitset << nl.
         ].
      ].
   ].
"===================================================================="


"===================================================================="
" OPTION -c: compress and write to output file / stdout "
"===================================================================="
   Copt ifTrue: [
      |readfile|
      readfile := FileStream open: infile mode: FileStream read.
      writefile = nil

      " Check: outfile exist ? "
      ifTrue: [
         writefile := stdout.
      ]
      ifFalse: [
         writefile := FileStream open: outfilename 
         mode: FileStream write.
      ].      
      " Encode version of each byte "
      " clone bits to outfile "
      extnode := ''.
      root postorder: [:char :isleaf |
         isleaf ifTrue: [
            tmp := ''.
            char = 0
            ifTrue: [tmp := '000000000' ] " Set osclone to 0 "
            ifFalse: [ char = 256
               ifTrue: [tmp := '000000001' ] " Set osclone to 1 "
               ifFalse: [" otherwise write remaining unused bits "
                  8 to: 1 by: -1 
                  do: [:i |
                     tmp := tmp, 
                     ((char bitAt: i) printString).
                  ]
               ].
            ].
            extnode := (extnode, '0', tmp).
         ]
         " Not External Node "
         ifFalse: [ 
            extnode := (extnode, '1').
         ].
      ].
      " Clone bits to osclone "
      osclone := OutBits new: writefile.
      1 to: (extnode size) 
      do: [:i | 
         copybitchunk := ((extnode at: i) asInteger) - 48.
         osclone writeBit: copybitchunk.
      ].
      osclone writeBit: 1.
      " Encode value copied to outfile, then write encoded
      version of each byte "
      [readfile atEnd not] 
      whileTrue: [
         |temp  bitwise key|
         key := readfile next asInteger.
         temp := encode at: key.
         temp 
         do: [:char |
            bitwise := 
            char asInteger - 48.
            osclone writeBit: bitwise.
         ].
      ].
      eoftrue := encode at: 256.
      eoftrue 
      do: [:char |
         bitwise := char asInteger - 48.
         osclone writeBit: bitwise.
      ].
      " Flush byte for size of byte  "
      osclone flushByte. 
   ].
].
"==================================================================="

"==================================================================="
" OPT -u: take compressed file as input, output uncompressed file   "
"==================================================================="
Uopt ifTrue: [
   |buildtree stack eof|
   Dopt ifTrue: [
      (writefile = nil) ifTrue: [
         stdout << 'NIL writefile Error (4): Print to standard 
                                                      output' << nl.
      ]
       ifFalse: [
          stdout << 'Printing to ' << outfilename << nl.
       ].
       stdout << '------Decompression------' << nl.
   ].
   writefile = nil
   ifTrue: [ writefile := stdout. ].
   stack := OrderedCollection new.

   " Open a BitStream containing input file "
   fileBits := BitStream new: inputfile.
   eof := replenish := false.
   buildtree := rNode := ''.
   
   " init first byte "
   charByte := 0.

   " Restock Tree "
   [(fileBits atEnd not) & (eof not)] whileTrue: [
      charByte := 0.
      currBit1 := fileBits nextBit.
      replenish ifFalse: [

         " if the current bit is 0, read the next byte "
         currBit1 = 0 ifTrue: [
            8 to: 1 by: -1 do: [:i |
               currBit2 := fileBits nextBit.
               charByte := charByte bitAt: i put: currBit2.
            ].

            " if this bit is 0, read one more bit "
            charByte = 0 ifTrue: [
               currBit2 := fileBits nextBit.

            " if this bit is 1, put 257 for EOF in charByte "
               currBit2 = 1 ifTrue: [
                  charByte := charByte + 256.
               ].
            ].
      
         stack add: (Leaf new: charByte count: 0).
         ].

         " if the current bit is 1 "
         currBit1 = 1 ifTrue: [
            rTree := stack removeLast.
            
            stack size = 0 ifTrue: [  
               " If you have removed the last tree "
               buildtree := rTree.
               rNode := buildtree.
               replenish := true.
            ]
            ifFalse: [ 
               " if there are more trees to remove "
               lTree := stack removeLast.
               newTree := Tree new: 
               lTree char count: 0 left: lTree right: rTree.
               stack add: newTree.
            ].
         ].
      ]
      " Reconstruct the Tree "
      ifTrue: [ 
         (rNode isleaf) ifTrue: [
            " When you reach eof external node, end the loop "
            rNode char = 256 ifTrue: [ eof := true ]
            ifFalse: [
               rNodeChar := Character value: rNode char.
               writefile nextPut: rNodeChar.
               rNode := buildtree.
            ].
         ]
         " if it's not an External Node "
         ifFalse: [
            currBit1 = 0 ifTrue: [   " if 0, move left "
               rNode := rNode left.
               rNode isleaf ifTrue: [
                  eof ifFalse: [
                     rNode char = 256 ifTrue: [ eof := true ]
                     ifFalse: [
                        rNodeChar := Character value: rNode char.
                        writefile nextPut: rNodeChar.
                        rNode := buildtree.
                     ].
                  ]
               ]
            ]
            " if 1, move right "
            ifFalse: [   
               rNode := rNode right.
               rNode isleaf ifTrue: [
                  rNode char ~= 256 ifFalse: [ eof := true ]
                  ifTrue: [
                     rNodeChar := Character value: rNode char.
                     writefile nextPut: rNodeChar.
                     rNode := buildtree.
                  ]
               ]
            ].
         ].
      ].
   ].
].
"===================================================================="

inputfile close.
(writefile notNil) ifTrue: [
   writefile close.
].
ObjectMemory quit: 0
"===================================================================="

