;
;			ui512a
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;
;			File:			ui512a.asm
;			Author:			John G. Lynch
;			Legal:			Copyright @2024, per MIT License below
;			Date:			May 13, 2024
;
;			Notes:
;				ui512 is a small project to provide basic operations for a variable type of unsigned 512 bit integer.
;
;				ui512a provides basic operations: zero, copy, compare, add, subtract.
;				ui512b provides basic bit-oriented operations: shift left, shift right, and, or, not, least significant bit and most significant bit.
;               ui512md provides multiply and divide.
;
;				It is written in assembly language, using the MASM (ml64) assembler provided as an option within Visual Studio.
;				(currently using VS Community 2022 17.14.10)
;
;				It provides external signatures that allow linkage to C and C++ programs,
;				where a shell/wrapper could encapsulate the methods as part of an object.
;
;				It has assembly time options directing the use of Intel processor extensions: AVX4, AVX2, SIMD, or none:
;				(Z (512), Y (256), X (128) registers, or regular Q (64bit)).
;
;				If processor extensions are used, the caller must align the variables declared and passed
;				on the appropriate byte boundary (e.g. alignas 64 for 512)
;
;				This module is very light-weight (less than 1K bytes) and relatively fast,
;				but is not intended for all processor types or all environments. 
;
;				Use for private (hobbyist), or instructional, or as an example for more ambitious projects.
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;
;			MIT License
;
;			Copyright (c) 2024 John G. Lynch
;
;				Permission is hereby granted, free of charge, to any person obtaining a copy
;				of this software and associated documentation files (the "Software"), to deal
;				in the Software without restriction, including without limitation the rights
;				to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;				copies of the Software, and to permit persons to whom the Software is
;				furnished to do so, subject to the following conditions:
;
;				The above copyright notice and this permission notice shall be included in all
;				copies or substantial portions of the Software.
;
;				THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;				IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;				FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;				AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;				LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;				OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;				SOFTWARE.
;
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------

				INCLUDE			ui512aMacros.inc
				OPTION			casemap:none

ui512D			SEGMENT			'DATA'	ALIGN (64)					; Declare a data segment	
				MemConstants										; Generate memory resident constants
ui512D			ENDS												; end of data segment

				VerifyRegs											; if option is turned on (in macros include),
																	; generate a debug routine for register integrity validation
				
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			zero_u		-	fill supplied 512bit (8 QWORDS) with zero
;			Prototype:		extern "C" void zero_u ( u64* destarr );
;			destarr		-	Address of destination 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			returns		-	nothing
;
				Leaf_Entry		zero_u, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				Zero512			RCX									; Zero 512 bit space addressed in RCX (the parameter)
				RET	
				Leaf_End		zero_u, ui512						; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			copy_u		-	copy supplied 512bit (8 QWORDS) source to supplied destination
;			Prototype:		extern "C" void copy_u( u64* destarr, u64* srcarr )
;			destarr		-	Address of destination 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			srcarr		-	Address of source 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	nothing
;
				Leaf_Entry		copy_u, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				Copy512			RCX, RDX							; Copy 512 bit space from scr array (address in RDX) to dest (RCX)
				RET	
				Leaf_End		copy_u, ui512						; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			setuT64		-	set supplied destination 512 bit to supplied u64 value
;			Prototype:		extern "C" void set_uT64( u64* destarr, u64 value )
;			destarr		-	Address of destination 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			src			-	u64 value in RDX
;			returns		-	nothing
;
				Leaf_Entry		set_uT64, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				Zero512			RCX	
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RDX
				RET	
				Leaf_End		set_uT64, ui512						; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			compare_u	-	unsigned compare supplied 512bit (8 QWORDS) LH operand to supplied RH operand
;			Prototype:		extern "C" s32 compare_u( u64* lh_op, u64* rh_op )
;			lh_op		-	Address of LH 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	Address of RH 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	(0) for equal, -1 for lh_op is less than rh_op, 1 for lh_op is greater than rh_op 
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		compare_u, ui512					; Declare code section, public proc, no prolog, no frame, exceptions handled by caller

				CheckAlign		RCX
				CheckAlign		RDX

	IF __UseZ
				VMOVDQA64		ZMM30, ZM_PTR [ RCX ]				; Load parameters
				VMOVDQA64		ZMM31, ZM_PTR [ RDX ]
				VPCMPUQ			K1, ZMM30, ZMM31, CPLT				; in-lane compare 8 words for 'less than'
				KMOVW			R8D, K1
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				SHL				R8D, 1								; shift to get bits 2 through 8
				BSF				R8D, R8D							; get bit number of right-most (most significant) 1 thru 8
				VPCMPUQ			K2, ZMM30, ZMM31, CPGT				; do the same for 'greater than'
				KMOVW			EAX, K2
				OR				EAX, MASK kMask.b8					; OR in a high bit to make an equal compare not zero				
				SHL				EAX, 1
				BSF				EAX, EAX
				CMP				R8D, EAX							; compare: which is most significant? LT or GT? (or zero - equal)
				MOV				EAX, ret0
				CMOVA			EAX, ret1
				CMOVB			EAX, ret_1
				RET

	ELSEIF	__UseY
				VMOVDQA64		YMM0, YM_PTR [ RCX ] [ 4 * 8 ]		; load most significant 4 qwords of parameters
				VMOVDQA64		YMM2, YM_PTR [ RDX ] [ 4 * 8 ]
				VPCMPUQ			K1, YMM0, YMM2, CPLT				; in-lane compare
				KMOVB			R8D, K1
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				SHL				R8D, 1								; shift so zero bit is one bit (so it doesnt get lost in BSR)
				BSF				R8D, R8D							; find most significant "LT" word
				VPCMPUQ			K1, YMM0, YMM2, CPGT				; repeat for "GT"
				KMOVB			EAX, K1
				OR				EAX, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				SHL				EAX, 1
				BSF				EAX, EAX
				CMP				R8D, EAX							; most significant (eiither LT or GT), else fall through to look at least significant 4 qwords
				JNE				@F
				VMOVDQA64		YMM1, YM_PTR [ RCX ] [ 0 * 8 ]
				VMOVDQA64		YMM3, YM_PTR [ RDX ] [ 0 * 8 ]
				VPCMPUQ			K2, YMM1, YMM3, CPLT
				KMOVB			R8D, K2
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				SHL				R8D, 1								; shift so zero bit is one bit (so it doesnt get lost in BSR)
				BSF				R8D, R8D
				VPCMPUQ			K2, YMM1, YMM3, CPGT
				KMOVB			EAX, K2
				OR				EAX, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				SHL				EAX, 1
				BSF				EAX, EAX
				CMP				R8D, EAX	
@@:
				MOV				EAX, ret0
				CMOVA			EAX, ret1
				CMOVB			EAX, ret_1
				RET

	ELSE

; FOR EACH index of 0 thru 7 : fetch qword of lh_op, compare to qword of rh_op; jump to exit if not equal
				FOR				idx, < 0, 1, 2, 3, 4, 5, 6, 7 >
				MOV				RAX, Q_PTR [ RCX ] [ idx * 8 ]
				CMP				RAX, Q_PTR [ RDX ] [ idx * 8 ]
				JNZ				@F
				ENDM

@@:
				MOV				RAX, 0
				CMOVA			EAX, ret1							; 'above' is greater than for an unsigned integer
				CMOVB			EAX, ret_1							; 'below' is less than for an unsigned integer
				RET
	ENDIF
				Leaf_End		compare_u, ui512					; end of proc, end of section 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			compare_uT64-	unsigned compare supplied 512bit (8 QWORDS) LH operand to supplied 64bit RH operand
;			Prototype:		extern "C" s32 compare_uT64( u64* lh_op, u64 rh_op )
;			lh_op		-	Address of LH 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	The RH 64-bit value in RDX
;			returns		-	(0) for equal, -1 for less than, 1 for greater than
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		compare_uT64, ui512					; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				
				CheckAlign		RCX

	IF		__UseZ
				VMOVDQA64		ZMM30, ZM_PTR [ RCX ]				; Load lh-op parameter
				LEA				RAX, MaskBit7
				KMOVB			K1, RAX
				VPBROADCASTQ 	ZMM31 {k1}{z}, RDX					; load rh_op parameter (both now in Z regs)
				VPCMPUQ			K1, ZMM30, ZMM31, CPLT				; in-lane compare for LT
				KMOVW			R8D, K1
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				SHL				R8D, 1								; shift to get bits 2 through 8
				BSF				R8D, R8D							; get bit number of right-most (most significant) 1 thru 8
				VPCMPUQ			K2, ZMM30, ZMM31, CPGT				; do the same for 'greater than'
				KMOVW			EAX, K2
				OR				EAX, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				SHL				EAX, 1
				BSF				EAX, EAX
				CMP				R8D, EAX							; compare: which is most significant? LT or GT? (or zero - equal)
				MOV				EAX, ret0
				CMOVA			EAX, ret1
				CMOVB			EAX, ret_1
				RET

	ELSE
				XOR				RAX, RAX
; FOR EACH index 0 thru 6: Get minuend QWORD, compare for zero 
				FOR				idx, < 0, 1, 2, 3, 4, 5, 6 >
				CMP				Q_PTR [ RCX ] [ idx * 8 ], RAX
				JNZ				@F
				ENDM

				MOV				RAX, Q_PTR [ RCX ] [ 7 * 8 ]
				CMP				RAX, RDX 
				JNZ				@F
				XOR				EAX, EAX
@@:				CMOVA			EAX, ret1							; 'above' is greater than for an unsigned integer
				CMOVB			EAX, ret_1							; 'below' is less than for an unsigned integer
				RET
	ENDIF
				Leaf_End		compare_uT64, ui512					; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			add_u		-	unsigned add supplied 512bit (8 QWORDS) sources to supplied destination
;			Prototype:		extern "C" s32 add_u( u64* sum, u64* addend1, u64* addend2 )
;			sum			-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			addend1		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			addend2		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no carry, 1 for carry (overflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		add_u, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller

				CheckAlign		RCX
				CheckAlign		RDX
				CheckAlign		R8	

	IF	__UseZ
;
;	__UseZ approach
;		Load operands into ZMM regs. Do in-lane qword adds (simultaneously)
;		Compare result to addend to see if any carries (in-lane simultaneous compare)
;		If there are carries: determine which lanes, add 1 to next most significant word(s) 
;		Special case: if highest order word carried, set overflow
;		Add of carries may cause additional carries, so repeat until no carries
;
;		Best case: no carries: 12 instructions.
;		Worst case: 0xFFF...FFF + 1: an eight word cascading carry all the way to overflow: loops seven times - 82 instructions.
;
; Load operands				
				VMOVDQA64		ZMM30, ZM_PTR [RDX]					; ZMM30 = addend1 (8 QWORDs)
				VMOVDQA64		ZMM31, ZM_PTR [R8]					; ZMM31 = addend2 (8 QWORDs)

; Set up loop variables: R9 to be broadcast for adding carries (a one), RAX for tracking the carries
				XOR				R9, R9
				INC				R9
				XOR				RAX, RAX							; Carry flag and return code, persistant through iterations

; Initial addition
				VPADDQ		    ZMM29, ZMM30, ZMM31					; ZMM29 = addend1 + addend2 (lane-by-lane)

; Compute carries from (first) in-lane addition
				VPCMPUQ		    K1, ZMM29, ZMM30, CPLT				; k1[i] = 1 if sum[i] < addend1[i] (carry out of lane i)

; Examine computed carries: Most sig bit? indicates overall carry overflow; shift to align, if then none? we are done; 
@@checkcarry:
				KMOVB			R8, K1								; Mask bits results (from compare above) in k1 to R8 
				ANDN			R8, RAX, R8							; Ignore carries already added in. Do THESE carries, AND NOT carries already done
				OR				RAX, R8								; Keep these carries, along with the prior carries
				SHR				R8, 1								; Shift right: carry-in for each lane (from lane i+1 to i)	
				JZ				@@saveexit							; if, after alignment shift, there are no carries: save and exit

; anything else, and for as long as these carrry additions cause additional carries, add one to each carried into SIMD lane
				KMOVB			K1, R8
				VPBROADCASTQ	ZMM28 { k1 } { z }, R9				; ZMM28 = carry-ins broadcast to selected lanes, zero non-selected lanes
				VPADDQ			ZMM29 { k1 }, ZMM29, ZMM28			; Add carry-ins to selected lanes
				VPCMPUQ			K1 { k1 }, ZMM29, ZMM28, CPLT		; k2[i] = 1 if new sum[i] < carry-in[i] (new carries) Compute any new carries
				JMP				@@checkcarry

; store final sum
@@saveexit:
				AND				RAX, 1								; Bit 1 signifies a carry out of most significant word (an overflow) -> return code
				VMOVDQA64		ZM_PTR [RCX], ZMM29					; Store final sum
				RET													; EAX carries return code (from carry computation above)

	ELSE
; if not using "Z" SIMD, then use Q regs, roughly 27 instructions counting return code, return				
				MOV				RAX, Q_PTR [ RDX ] [ 7 * 8 ]
				ADD				RAX, Q_PTR [ R8 ] [ 7 * 8 ]
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RAX

; FOR EACH index of 6 thru 0 : fetch qword of addend1 (RDX), add (with carry) to qword of addend2; store at callers sum			
				FOR				idx, < 6, 5, 4, 3, 2, 1, 0 >
				MOV				RAX, Q_PTR [ RDX ] [ idx * 8 ]
				ADCX			RAX, Q_PTR [ R8 ] [ idx * 8 ]
				MOV				Q_PTR [ RCX ] [ idx * 8 ] , RAX
				ENDM

				MOV				RAX, 0								; return carry flag as overflow
				CMOVC			EAX, ret1
				RET	

	ENDIF
				Leaf_End		add_u, ui512						; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			add_uT64	-	add supplied 64bit QWORD (value) to 512bit (8 QWORDS), place in supplied destination
;			Prototype:		extern "C" s32 add_uT64( u64* sum, u64* addend1, u64 addend2 )
;			sum			-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			addend1		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			addend2		-	The 64-bit value in R8
;			returns		-	zero for no carry, 1 for carry (overflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		add_uT64, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller

				CheckAlign		RCX
				CheckAlign		RDX

	IF __UseZ
; Load operands				
				VMOVDQA64		ZMM30, ZM_PTR [RDX]					; ZMM30 = addend1 (8 QWORDs)
				KMOVB			K1, mskB7							; mask for least significant word
				VPBROADCASTQ	ZMM31 { k1 } { z }, R8				; ZMM31 now 512 bit version of passed addend2

; Set up loop variables: R9 to be broadcast for adding carries, RAX for tracking all the carries
				XOR				R9, R9
				INC				R9
				XOR				RAX, RAX							; Carry flag and return code, persistant through iterations

; Initial addition
				VPADDQ		    ZMM29, ZMM30, ZMM31					; ZMM29 = addend1 + addend2 (lane-wise)

; Compute carries from (first) in-lane addition
				VPCMPUQ		    K1, ZMM29, ZMM30, CPLT				; k1[i] = 1 if sum[i] < addend1[i] (carry out of lane i)

; Examine computed carries: MSB? indicates overall carry overflow; shift to align, if then none? we are done; 
@@checkcarry:
				KMOVB			R8, K1								; Mask bits results (from compare above) in k1 to R8
				ANDN			R8, RAX, R8							; Ignore carries already added in. Do these carries, AND NOT carries already done
				OR				RAX, R8								; Keep these carries, along with the prior carries
				SHR				R8, 1								; Shift right: carry-in for each lane (from lane i+1 to i)	
				JZ				@@saveexit							; If, after alignment shift, there are no carries, save and exit

; anything else, and for as long as these additions cause carries, add one to each carried into SIMD lane
				KMOVB			K1, R8
				VPBROADCASTQ	ZMM28 { k1 } { z }, R9				; ZMM28 = carry-ins broadcast to selected lanes, zero non-selected lanes
				VPADDQ			ZMM29 { k1 }, ZMM29, ZMM28			; Add carry-ins to selected lanes
				VPCMPUQ			K1 { k1 }, ZMM29, ZMM28, CPLT		; k2[i] = 1 if new sum[i] < carry-in[i] (new carries) Compute any new carries
				JMP				@@checkcarry

; store final sum
@@saveexit:
				AND				RAX, 1								; Bit 1 signifies a carry out of most significant word (an overflow) -> return code
				VMOVDQA64		ZM_PTR [RCX], ZMM29					; Store final sum
				RET													; EAX carries return code (from carry computation above)
	ELSE

; First Addition, Get Least significant QWORD of addend, Add passed QWORD to it
				MOV				RAX, Q_PTR [ RDX ] [ 7 * 8 ]
				ADD				RAX, R8 
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RAX
; FOR EACH index 6 thru 0: Get addend QWORD, add zero, but with carry if any from previous add
				FOR				idx, < 6, 5, 4, 3, 2, 1, 0 >
				MOV				RAX, Q_PTR [ RDX ] [ idx * 8 ]
				ADCX			RAX, zeroQ
				MOV				Q_PTR [ RCX ] [ idx * 8 ], RAX
				ENDM

; return zero unless carry still exists from addition
				MOV				EAX, ret0
				CMOVC			EAX, ret1
				RET	
	ENDIF

				Leaf_End		add_uT64, ui512						; end of proc, end of section 
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			sub_u		-	subtract supplied 512bit (8 QWORDS) RH OP from LH OP giving difference in destination
;			Prototype:		extern "C" s32 sub_u( u64* difference, u64* left operand, u64* right operand )
;			difference	-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of the LHOP 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	Address of the RHOP 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		sub_u, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller

				CheckAlign		RCX
				CheckAlign		RDX
				CheckAlign		R8

	IF __UseZ

; Load operands
				VMOVDQA64		ZMM30, ZM_PTR [RDX]					; Load lh_op
				VMOVDQA64		ZMM31, ZM_PTR [R8]					; Load rh_op

; Initialize loop variables: R9 for the targeted in-lane subract of borrows; RAX for return code overall borrow flag
				XOR				R9, R9
				INC				R9
				XOR				RAX, RAX

; Initial subraction
				VPSUBQ		    ZMM29, ZMM30, ZMM31					; Initial subtraction

; Compute initial borrows
				VPCMPUQ		    K1, ZMM30, ZMM31, CPLT				; Initial borrows

; Examine computed borrows: 
@@checkborrow:
				KMOVB			R8, K1								; Mask bits results (from compare above) in k1 to R10 (and R8)
				ANDN			R8, RAX, R8							; Ignore borrows already done. Do these borrows, AND NOT borrows already done
				OR				RAX, R8								; Keep these borrows, along with the prior borrows
				SHR				R8, 1								; Shift right: borrow-from for each lane (from lane i+1 to i)	
				JZ				@@saveexit							; If, after alignment shift, there are no borrows, save and exit

; anything else, and for as long as these subtractions cause borrows, subtract one from each borrowed from SIMD lane
				KMOVB			K1, R8
				VPBROADCASTQ	ZMM28 { k1 }  {z }, R9				; Apply borrow-ins only where needed
				VPSUBQ			ZMM29 { k1 }, ZMM29, ZMM28
				VPCMPUQ			k1 { k1 }, ZMM29, ZMM28, CPGT		; compute new mask of borrows
				JMP				@@checkborrow
@@saveexit:
				AND				RAX, 1
				VMOVDQA64		ZM_PTR [RCX], ZMM29
				RET

	ELSE

				MOV				RAX, Q_PTR [ RDX ] [ 7 * 8 ]		; get last word of minuend (least significant word of the number we are subtracting from) (left-hand operand)
				SUB				RAX, Q_PTR [ R8 ] [ 7 * 8 ]			; subtract last word of subrahend (the number to be subtracted) (right-hand operand)
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RAX		; store result in last word of difference, note: the flag 'carry' has been set to whether there has been a 'borrow'

; FOR EACH index 6 thru 0: Get minuend QWORD, subtract (with borrow), store at difference
				FOR				idx, < 6, 5, 4, 3, 2, 1, 0 >
				MOV				RAX, [ RDX ] [ idx * 8 ]					
				SBB				RAX, [ R8 ] [ idx * 8 ]
				MOV				[ RCX ] [ idx * 8 ], RAX
				ENDM

				MOV				RAX, 0								; return, set return code to zero if no remaining borrow, to one if there is a borrow
				CMOVC			EAX, ret1
				RET
	ENDIF
				Leaf_End		sub_u, ui512						; end of proc, end of section 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			sub_uT64	-	subtract supplied 64 bit right hand (64 bit value) op from left hand (512 bit) giving difference
;			Prototype:		extern "C" s32 sub_uT64( u64* difference, u64* left operand, u64 right operand )
;			difference	-	Address of 64 byte alligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	64-bitvalue in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		sub_uT64, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller

				CheckAlign		RCX
				CheckAlign		RDX

	IF __UseZ

; Load operands
				VMOVDQA64		ZMM30, ZM_PTR [RDX]			        ; Load lh_op
				KMOVB			K1, mskB7							; mask for least significant word
				VPBROADCASTQ	ZMM31 { k1 } { z }, R8				; ZMM31 now 512 bit version of passed rh_op

; Initialize loop variables: R9 for the targeted in-lane subract of borrows; RAX for return code overall borrow flag
				XOR				R9, R9
				INC				R9
				XOR				RAX, RAX

; Initial subraction
				VPSUBQ		    ZMM29, ZMM30, ZMM31					; Initial subtraction

; Compute initial borrows
				VPCMPUQ		    K1, ZMM30, ZMM31, CPLT				; Initial borrows

; Examine computed borrows: 
@@checkborrow:
				KMOVB			R8, K1								; Mask bits results (from compare above) in k1 to R8
				ANDN			R8, RAX, R8							; Ignore borrows already done. Do these borrows, AND NOT borrows already done
				OR				RAX, R8								; Keep these borrows, along with the prior borrows
				SHR				R8, 1								; Shift right: borrow-from for each lane (from lane i+1 to i)	
				JZ				@@saveexit							; If, after alignment shift, there are no borrows, save and exit

; anything else, and for as long as these subtractions cause borrows, subtract one from each borrowed from SIMD lane
				KMOVB			K1, R8
				VPBROADCASTQ	ZMM28 { k1 }  {z }, R9				; Apply borrow-ins only where needed
				VPSUBQ			ZMM29 { k1 }, ZMM29, ZMM28
				VPCMPUQ			k1 { k1 }, ZMM29, ZMM28, CPGT		; compute new mask of borrows
				JMP				@@checkborrow
@@saveexit:
				AND				RAX, 1
				VMOVDQA64		ZM_PTR [RCX], ZMM29
				RET

	ELSE
				MOV				RAX, [ RDX ] [ 7 * 8 ]		; 
				SUB				RAX, R8
				MOV				[ RCX ] [ 7 * 8 ], RAX

; FOR EACH index 6 thru 0: Get minuend QWORD, subtract borrow (if any), store at difference
				FOR				idx, < 6, 5, 4, 3, 2, 1, 0 >
				MOV				RAX, [ RDX ] [ idx * 8 ]					
				SBB				RAX, 0
				MOV				[ RCX ] [ idx * 8 ], RAX
				ENDM

				MOV				RAX, 0
				CMOVC			EAX, ret1
				RET
	ENDIF

				Leaf_End		sub_uT64, ui512						; end of proc, end of section

				END