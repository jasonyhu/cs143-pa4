	.data
	.align	2
	.globl	class_nameTab
	.globl	Main_protObj
	.globl	Int_protObj
	.globl	String_protObj
	.globl	bool_const0
	.globl	bool_const1
	.globl	_int_tag
	.globl	_bool_tag
	.globl	_string_tag
	.globl	class_objTab
_int_tag:
	.word	2
_bool_tag:
	.word	3
_string_tag:
	.word	4
	.globl	_MemMgr_INITIALIZER
_MemMgr_INITIALIZER:
	.word	_NoGC_Init
	.globl	_MemMgr_COLLECTOR
_MemMgr_COLLECTOR:
	.word	_NoGC_Collect
	.globl	_MemMgr_TEST
_MemMgr_TEST:
	.word	0
	.word	-1
str_const2:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const1
	.byte	0	
	.align	2
	.word	-1
str_const1:
	.word	4
	.word	8
	.word	String_dispTab
	.word	int_const2
	.ascii	"<basic class>"
	.byte	0	
	.align	2
	.word	-1
str_const0:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const3
	.ascii	"example.cl"
	.byte	0	
	.align	2
	.word	-1
int_const3:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	10
	.word	-1
int_const2:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	13
	.word	-1
int_const1:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
int_const0:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	42
	.word	-1
bool_const0:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
bool_const1:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	1
class_nameTab:

	.word	Object
	.word	IO
	.word	Int
	.word	Bool
	.word	String
	.word	Main
class_objTab:

	.word	Object_protObj
	.word	Object_init
	.word	IO_protObj
	.word	IO_init
	.word	Int_protObj
	.word	Int_init
	.word	Bool_protObj
	.word	Bool_init
	.word	String_protObj
	.word	String_init
	.word	Main_protObj
	.word	Main_init
Object_dispTab:

	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
IO_dispTab:

	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
Int_dispTab:

	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
Bool_dispTab:

	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
String_dispTab:

	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	String.length
	.word	String.concat
	.word	String.substr
Main_dispTab:

	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	Main.main
Object_protObj:

	.word	0
	.word	Object_dispTab
	.word	-1
IO_protObj:

	.word	1
	.word	IO_dispTab
	.word	-1
Int_protObj:

	.word	2
	.word	Int_dispTab
	.word	0
	.word	-1
Bool_protObj:

	.word	3
	.word	Bool_dispTab
	.word	0
	.word	-1
String_protObj:

	.word	4
	.word	String_dispTab
	.word	0
	.word	
	.word	-1
Main_protObj:

	.word	5
	.word	Main_dispTab
	.word	-1
	.globl	heap_start
heap_start:
	.word	0
	.text
	.globl	Main_init
	.globl	Int_init
	.globl	String_init
	.globl	Bool_init
	.globl	Main.main
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	move	$a0 $zero
	la	$a0 int_const0
