;
;			ui512a
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;
;			File:			ui512a.asm
;			Author:			John G. Lynch
;			Legal:			Copyright @2024, per MIT License below
;			Date:			May 13, 2024

				INCLUDE			legalnotes.inc
				INCLUDE			compile_time_options.inc
				INCLUDE			ui512aMacros.inc
				OPTION			casemap:none

ui512D			SEGMENT			'DATA'	ALIGN (64)					; Declare a data segment, aligned	
				MemConstants										; Generate memory resident constants
ui512D			ENDS												; end of data segment

				VerifyRegs											; if option is turned on (in included macros file),
																	; generate a debug routine for register integrity validation

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			zero_u		-	fill supplied 512bit (8 QWORDS) with zero
;			Prototype:		extern "C" void zero_u ( u64* destarr );
;			destarr		-	Address of destination 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
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
;			destarr		-	Address of destination 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			srcarr		-	Address of source 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	nothing
;
				Leaf_Entry		copy_u, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				Copy512			RCX, RDX							; Copy 512 bit space from src array (address in RDX) to dest (RCX)
				RET	
				Leaf_End		copy_u, ui512						; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			set_uT64	-	set supplied destination 512 bit to supplied u64 value
;			Prototype:		extern "C" void set_uT64( u64* destarr, u64 value )
;			destarr		-	Address of destination 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			src			-	u64 value in RDX
;			returns		-	nothing
;
				Leaf_Entry		set_uT64, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				CheckAlign		RCX									; (OUT) destination array to be set

	IF __UseZ
				KMOVB			k1, mskB7							; Mask for least significant word (Note: ZMM regs store least first, while mem stores least last. Beware.)
				VPBROADCASTQ 	ZMM31 {k1}{z}, RDX					; load parameter, zeroing all other qwords
				VMOVDQA64		ZM_PTR [ RCX ], ZMM31				; store at destination

	ELSE
				Zero512			RCX									; Zero destination array	
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RDX		; Move given 64 bit qwored to least significant qword of array

	ENDIF
				RET	
				Leaf_End		set_uT64, ui512						; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			compare_u	-	unsigned compare supplied 512bit (8 QWORDS) LH operand to supplied RH operand
;			Prototype:		extern "C" s32 compare_u( u64* lh_op, u64* rh_op )
;			lh_op		-	Address of LH 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	Address of RH 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			returns		-	(0) for equal, -1 for lh_op is less than rh_op, 1 for lh_op is greater than rh_op 
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		compare_u, ui512					; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				CheckAlign		RCX									; (IN) left hand (LH) operand to compare
				CheckAlign		RDX									; (IN) right hand (RH) operand in the compare 

	IF __UseZ
				VMOVDQA64		ZMM30, ZM_PTR [ RCX ]				; Load parameters
				VMOVDQA64		ZMM31, ZM_PTR [ RDX ]
				VPCMPUQ			k1, ZMM30, ZMM31, CPLT				; in-lane compare 8 words for 'less than'
				VPCMPUQ			k2, ZMM30, ZMM31, CPGT				; do the same for 'greater than' (interleave these two compares to hide latencies)
				KMOVW			R8D, k1
				KMOVW			EAX, k2
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				OR				EAX, MASK kMask.b8
				SHL				R8D, 1								; shift to get bits 2 through 8
				SHL				EAX, 1
		IF __UseBMI2
				TZCNT			R8D, R8D							; get bit number of right-most (most significant) 1 thru 8
				TZCNT			EAX, EAX
		ELSE		
				BSF				R8D, R8D							; get bit number of right-most (most significant) 1 thru 8					
				BSF				EAX, EAX
		ENDIF
				CMP				R8D, EAX							; compare: which is most significant? LT or GT? (or zero - equal)
				LEA				EAX, [ retcode_zero ]
				CMOVA			EAX, ret_one
				CMOVB			EAX, ret_neg_one
				RET

	ELSEIF	__UseY
				VMOVDQA64		YMM0, YM_PTR [ RCX ] [ 4 * 8 ]		; load most significant 4 qwords of parameters
				VMOVDQA64		YMM2, YM_PTR [ RDX ] [ 4 * 8 ]
				VPCMPUQ			k1, YMM0, YMM2, CPLT				; in-lane compare, this one for 'LT'
				VPCMPUQ			k2, YMM0, YMM2, CPGT				; repeat for "GT"
				KMOVB			R8D, k1								; LT compare result to R8D
				KMOVB			EAX, k2								; GT compare result to EAX
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero
				OR				EAX, MASK kMask.b8
				SHL				R8D, 1								; shift so zero bit is one bit 
				SHL				EAX, 1
		IF __UseBMI2
				TZCNT			R8D, R8D							; get bit number of right-most (most significant) 1 thru 8
				TZCNT			EAX, EAX
		ELSE
				BSF				R8D, R8D							; find most significant "LT" word
				BSF				EAX, EAX							; same for 'GT' word
		ENDIF
				CMP				R8D, EAX							; most significant (either LT or GT), else fall through to look at least significant 4 qwords
				JNE				@F
				VMOVDQA64		YMM1, YM_PTR [ RCX ] [ 0 * 8 ]		; if the most significant 4 qwords were equal, have to look at least significant 4 qwords
				VMOVDQA64		YMM3, YM_PTR [ RDX ] [ 0 * 8 ]
				VPCMPUQ			k1, YMM1, YMM3, CPLT
				VPCMPUQ			k2, YMM1, YMM3, CPGT
				KMOVB			R8D, k1
				KMOVB			EAX, k2
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				OR				EAX, MASK kMask.b8
				SHL				R8D, 1								; shift so zero bit is one bit
				SHL				EAX, 1
		IF __UseBMI2
				TZCNT			R8D, R8D							; get bit number of right-most (most significant) 1 thru 8
				TZCNT			EAX, EAX
		ELSE
				BSF				R8D, R8D							; find most significant "LT" word
				BSF				EAX, EAX							; same for 'GT' word
		ENDIF
				CMP				R8D, EAX	
@@:
				LEA				EAX, [ retcode_zero ]
				CMOVA			EAX, ret_one
				CMOVB			EAX, ret_neg_one
				RET

	ELSE
; FOR EACH index of 0 thru 7 : fetch qword of lh_op, compare to qword of rh_op; jump to exit if not equal
				FOR				idx, < 0, 1, 2, 3, 4, 5, 6, 7 >
				MOV				RAX, Q_PTR [ RCX ] [ idx * 8 ]
				CMP				RAX, Q_PTR [ RDX ] [ idx * 8 ]
				JNZ				@F
				ENDM
@@:
				LEA				RAX, [ retcode_zero ]
				CMOVA			EAX, ret_one						; 'above' is greater than for an unsigned integer
				CMOVB			EAX, ret_neg_one					; 'below' is less than for an unsigned integer
				RET

	ENDIF
				Leaf_End		compare_u, ui512					; end of proc, end of section 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			compare_uT64-	unsigned compare supplied 512bit (8 QWORDS) LH operand to supplied 64bit RH operand
;			Prototype:		extern "C" s32 compare_uT64( u64* lh_op, u64 rh_op )
;			lh_op		-	Address of LH 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			rh_op		-	The RH 64-bit value in RDX
;			returns		-	(0) for equal, -1 for less than, 1 for greater than
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		compare_uT64, ui512					; Declare code section, public proc, no prolog, no frame, exceptions handled by caller				
				CheckAlign		RCX									; (IN) left hand (LH) operand to compare 

	IF		__UseZ
				VMOVDQA64		ZMM30, ZM_PTR [ RCX ]				; Load lh-op parameter
				KMOVB			k1, mskB7
				VPBROADCASTQ 	ZMM31 {k1}{z}, RDX					; load rh_op parameter (both now in Z regs)
				VPCMPUQ			k1, ZMM30, ZMM31, CPLT				; in-lane compare for LT
				VPCMPUQ			k2, ZMM30, ZMM31, CPGT				; do the same for 'greater than'
				KMOVW			R8D, k1
				KMOVW			EAX, k2
				OR				R8D, MASK kMask.b8					; OR in a high bit to make an equal compare not zero	
				OR				EAX, MASK kMask.b8
				SHL				R8D, 1								; shift to get bits 2 through 8
				SHL				EAX, 1
		IF __UseBMI2
				TZCNT			R8D, R8D							; get bit number of right-most (most significant) 1 thru 8
				TZCNT			EAX, EAX
		ELSE
				BSF				R8D, R8D							; find most significant "LT" word
				BSF				EAX, EAX							; same for 'GT' word
		ENDIF
				CMP				R8D, EAX							; compare: which is most significant? LT or GT? (or zero - equal)
				LEA				EAX, [ retcode_zero ]
				CMOVA			EAX, ret_one
				CMOVB			EAX, ret_neg_one
				RET

	ELSE
				XOR				RAX, RAX
; FOR EACH index 0 thru 6: Get QWORD, compare for zero 
				FOR				idx, < 0, 1, 2, 3, 4, 5, 6 >
				CMP				Q_PTR [ RCX ] [ idx * 8 ], RAX
				JNZ				@F
				ENDM

				MOV				RAX, Q_PTR [ RCX ] [ 7 * 8 ]
				CMP				RAX, RDX 
				JNZ				@F
				XOR				EAX, EAX
@@:				CMOVA			EAX, ret_one						; 'above' is greater than for an unsigned integer
				CMOVB			EAX, ret_neg_one					; 'below' is less than for an unsigned integer
				RET

	ENDIF
				Leaf_End		compare_uT64, ui512					; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			add_u		-	unsigned add supplied 512bit (8 QWORDS) sources to supplied destination
;			Prototype:		extern "C" s32 add_u( u64* sum, u64* addend1, u64* addend2 )
;			sum			-	Address of 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			addend1		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			addend2		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no carry, 1 for carry (overflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		add_u, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				CheckAlign		RCX									; (OUT) 8 QWORD sum
				CheckAlign		RDX									; (IN) 8 QWORD LH addend
				CheckAlign		R8									; (IN) 8 QWORD RH addend

     IF __UseZ
; Load operands
                VMOVDQA64		ZMM30, ZM_PTR [ RDX ]				; Load addend1 (lh_op)
                VMOVDQA64		ZMM31, ZM_PTR [ R8 ]				; Load addend2 (rh_op)

; Initialize: R9=1 for carry-add; K7=0 for carry done mask
                XOR				R9, R9
                INC				R9
                KXORB			k7, k7, k7							; Clear k7 (what carries have been done mask)

; Initial addition and carry computation (interleaved for pipeline)
                VPADDQ			ZMM29, ZMM30, ZMM31					; Initial sum, lane by lane add
                VPCMPUQ			k1, ZMM29, ZMM30, CPLT				; Initial carries: sum[i] < lh_op[i] (unsigned carry detect, lane by lane)

; Carry propagation (loop until done) Note: tried unrolled here, but code size, hence instruction pipeline fetch, slower than tight loop
@@:
                KANDNB			k2, k7, k1							; eliminate apparent carries if they had already been flagged (and processed) (by AND NOT previous K7)
                KORB			k7, k7, k2							; new carries in K2, OR into carries already done (K7)
                KSHIFTRB		k3, k2, 1							; new carries shift right to align which lane to add carry to
                KTESTB			k3, k3								; no new carries? exit
                JZ				@@saveexit
                VPBROADCASTQ	ZMM28 {k3}{z}, R9					; in scatch reg, zero lanes, load shifted carry lanes with '1'
                VPADDQ			ZMM29 {k3}, ZMM29, ZMM28			; add in the carries
                VPCMPUQ			k1 {k3}, ZMM29, ZMM28, CPLT			; compare result, lane by lane, to see if less than orginal (indicating an overflow / carry)
				JMP				@B									; check for additional (newly generated) carries

; Complete, extract carry out (overflow) for return code, store result sum at callers sum
@@saveexit:
                KMOVB			RAX, k7								; Move final carries done to RAX
                AND				RAX, retcode_one					; bit 0 (MSB carry-out)
                VMOVDQA64		ZM_PTR [ RCX ], ZMM29				; Store sum
                RET

 	ELSE
; if not using "Z" SIMD, then use Q regs, roughly 27 instructions counting return code, return				
				MOV				RAX, Q_PTR [ RDX ] [ 7 * 8 ]
				ADD				RAX, Q_PTR [ R8 ] [ 7 * 8 ]
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RAX

; FOR EACH index of 6 thru 0 : fetch qword of addend1 (RDX), add (with carry) to qword of addend2; store at callers sum			
				FOR				idx, < 6, 5, 4, 3, 2, 1, 0 >
				MOV				RAX, Q_PTR [ RDX ] [ idx * 8 ]
				ADCX			RAX, Q_PTR [ R8 ] [ idx * 8 ]		; Note: some CPUs dont support ADCX (64bit unsigned add) An Add with checking sign change could be used
				MOV				Q_PTR [ RCX ] [ idx * 8 ] , RAX
				ENDM

; Complete. Carry to return code
				LEA				RAX, [ retcode_zero ]				; return carry flag as overflow
				CMOVC			EAX, ret_one
				RET	

	ENDIF
				Leaf_End		add_u, ui512						; end of proc, end of section

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			add_uT64	-	add supplied 64bit QWORD (value) to 512bit (8 QWORDS), place in supplied destination
;			Prototype:		extern "C" s32 add_uT64( u64* sum, u64* addend1, u64 addend2 )
;			sum			-	Address of 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			addend1		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			addend2		-	The 64-bit value in R8
;			returns		-	zero for no carry, 1 for carry (overflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		add_uT64, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				CheckAlign		RCX									; (OUT) 8 QWORD sum
				CheckAlign		RDX									; (IN) 8 QWORD LH addend

	IF __UseZ
; Load operands				
				VMOVDQA64		ZMM30, ZM_PTR [RDX]					; ZMM30 = addend1 (8 QWORDs)
				KMOVB			k1, mskB7							; mask for least significant word
				VPBROADCASTQ	ZMM31 {k1}{z}, R8					; ZMM31 now 512 bit version of passed addend2

; Initialize: R9=1 for carry-add; K7=0 for carry done mask
                XOR				R9, R9
                INC				R9
                KXORB			k7, k7, k7							; Clear k7 (what carries have been done mask)

; Initial addition and carry computation (interleaved for pipeline)
                VPADDQ			ZMM29, ZMM30, ZMM31					; Initial sum, lane by lane add
                VPCMPUQ			k1, ZMM29, ZMM30, CPLT				; Initial carries: sum[i] < lh_op[i] (unsigned carry detect, lane by lane)

; Carry propagation (loop until done) Note: tried unrolled here, but code size, hence instruction pipeline fetch, slower than tight loop
@@:
                KANDNB			k2, k7, k1							; eliminate apparent carries if they had already been flagged (and processed) (AND NOT previous K7)
                KORB			k7, k7, k2							; new carries in K2, OR into carries done (K7)
                KSHIFTRB		k3, k2, 1							; new carries shift right to align which lane to add carry to
                KTESTB			k3, k3								; no new carries? exit
                JZ				@@saveexit
                VPBROADCASTQ	ZMM28 {k3}{z}, R9					; in scatch reg, zero lane, load shifted carry lanes with '1'
                VPADDQ			ZMM29 {k3}, ZMM29, ZMM28			; add in the carries
                VPCMPUQ			k1 {k3}, ZMM29, ZMM28, CPLT			; compare result, lane by lane, to see if less than orginal (indicating an overflow / carry)
				JMP				@B									; check for additional (newly generated) carries

; Complete, extract carry out (overflow) for return code, store result sum at callers sum
@@saveexit:
                KMOVB			RAX, k7								; Move final carries done to RAX
                AND				RAX, retcode_one					; bit 0 (MSB carry-out)
                VMOVDQA64		ZM_PTR [ RCX ], ZMM29				; Store sum
                RET													; EAX carries return code (from carry computation above)

	ELSE

; First Addition, Get Least significant QWORD of addend, Add passed QWORD to it
				MOV				RAX, Q_PTR [ RDX ] [ 7 * 8 ]
				ADD				RAX, R8 
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RAX
; FOR EACH index 6 thru 0: Get addend QWORD, add zero, but with carry if any from previous add
				FOR				idx, < 6, 5, 4, 3, 2, 1, 0 >
				MOV				RAX, Q_PTR [ RDX ] [ idx * 8 ]
				ADCX			RAX, zeroQ							; Note: some CPUs dont support ADCX (64bit unsigned add) An Add with checking sign change could be used
				MOV				Q_PTR [ RCX ] [ idx * 8 ], RAX
				ENDM

; return zero unless carry still exists from addition
				LEA				EAX, [ retcode_zero ]
				CMOVC			EAX, ret_one
				RET	

	ENDIF
				Leaf_End		add_uT64, ui512						; end of proc, end of section 
;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			sub_u		-	subtract supplied 512bit (8 QWORDS) RH OP from LH OP giving difference in destination
;			Prototype:		extern "C" s32 sub_u( u64* difference, u64* left operand, u64* right operand )
;			difference	-	Address of 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of the LHOP 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	Address of the RHOP 8 64-bit QWORDS (512 bits) in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		sub_u, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				CheckAlign		RCX									; (OUT) 8 QWORD difference
				CheckAlign		RDX									; (IN) 8 QWORD left hand operand, minuend
				CheckAlign		R8									; (IN) 8 QWORD right hand operand, subtrahend

	IF __UseZ
; Load operands
				VMOVDQA64		ZMM30, ZM_PTR [RDX]					; Load lh_op
				VMOVDQA64		ZMM31, ZM_PTR [R8]					; Load rh_op

; Initialize loop variables: R9 for the targeted in-lane subtract of borrows; RAX for return code overall borrow flag
				XOR				R9, R9
				INC				R9
				KXORB			k7, k7, k7							; Clear k7 (what borrows have been done mask)

; Initial subtraction
				VPSUBQ		    ZMM29, ZMM30, ZMM31					; Initial subtraction
				VPCMPUQ		    k1, ZMM30, ZMM31, CPLT				; Initial borrows

; Examine computed borrows: 
@@:
                KANDNB			k2, k7, k1							; eliminate apparent borrows if they had already been flagged (and processed) (AND NOT previous K7)
                KORB			k7, k7, k2							; new borrows in K2, OR into borrows done (K7)
                KSHIFTRB		k3, k2, 1							; new borrows shift right to align which lane to add borrow from
                KTESTB			k3, k3								; no new borrows? exit
				JZ				@@saveexit							; If, after alignment shift, there are no borrows, save and exit

; anything else, and for as long as these subtractions cause borrows, subtract one from each borrowed from SIMD lane
				VPBROADCASTQ	ZMM28 {k3}{z}, R9					; Apply borrow-ins only where needed
				VPSUBQ			ZMM29 {k3}, ZMM29, ZMM28
				VPCMPUQ			k1 { k3 }, ZMM29, ZMM28, CPGT		; compute new mask of borrows
				JMP				@B
@@saveexit:
                KMOVB			RAX, k7								; Move final borrows done to RAX
                AND				RAX, retcode_one					; bit 0 (MSB borrow-out)
				VMOVDQA64		ZM_PTR [RCX], ZMM29
				RET

	ELSE
				MOV				RAX, Q_PTR [ RDX ] [ 7 * 8 ]		; get last word of minuend (least significant word of the number we are subtracting from) (left-hand operand)
				SUB				RAX, Q_PTR [ R8 ] [ 7 * 8 ]			; subtract last word of subtrahend (the number to be subtracted) (right-hand operand)
				MOV				Q_PTR [ RCX ] [ 7 * 8 ], RAX		; store result in last word of difference, note: the flag 'carry' has been set to whether there has been a 'borrow'

; FOR EACH index 6 thru 0: Get minuend QWORD, subtract (with borrow), store at difference
				FOR				idx, < 6, 5, 4, 3, 2, 1, 0 >
				MOV				RAX, [ RDX ] [ idx * 8 ]					
				SBB				RAX, [ R8 ] [ idx * 8 ]
				MOV				[ RCX ] [ idx * 8 ], RAX
				ENDM

				LEA				RAX, [ retcode_zero ]				; return, set return code to zero if no remaining borrow, to one if there is a borrow
				CMOVC			EAX, ret_one
				RET

	ENDIF
				Leaf_End		sub_u, ui512						; end of proc, end of section 

;
;--------------------------------------------------------------------------------------------------------------------------------------------------------------
;			sub_uT64	-	subtract supplied 64 bit right hand (64 bit value) op from left hand (512 bit) giving difference
;			Prototype:		extern "C" s32 sub_uT64( u64* difference, u64* left operand, u64 right operand )
;			difference	-	Address of 64 byte aligned array of 8 64-bit words (QWORDS) 512 bits (in RCX)
;			lh_op		-	Address of  the 64 byte aligned array of 8 64-bit QWORDS (512 bits) in RDX
;			rh_op		-	64-bitvalue in R8
;			returns		-	zero for no borrow, 1 for borrow (underflow)
;			Note: unrolled code instead of loop: faster, and no regs to save / setup / restore
;
				Leaf_Entry		sub_uT64, ui512						; Declare code section, public proc, no prolog, no frame, exceptions handled by caller
				CheckAlign		RCX									; (OUT) 8 QWORD difference
				CheckAlign		RDX									; (IN) 8 QWORD left hand operand, minuend

	IF __UseZ
; Load operands
				VMOVDQA64		ZMM30, ZM_PTR [RDX]			        ; Load lh_op
				KMOVB			k1, mskB7							; mask for least significant word
				VPBROADCASTQ	ZMM31 {k1}{z}, R8					; ZMM31 now 512 bit version of passed rh_op

; Initialize loop variables: R9 for the targeted in-lane subtract of borrows; RAX for return code overall borrow flag
				XOR				R9, R9
				INC				R9
				KXORB			k7, k7, k7							; Clear k7 (what borrows have been done mask)

; Initial subtraction
				VPSUBQ		    ZMM29, ZMM30, ZMM31					; Initial subtraction
				VPCMPUQ		    k1, ZMM30, ZMM31, CPLT				; Initial borrows

; Examine computed borrows: 
@@:
                KANDNB			k2, k7, k1							; eliminate apparent borrows if they had already been flagged (and processed) (AND NOT previous K7)
                KORB			k7, k7, k2							; new borrows in K2, OR into borrows done (K7)
                KSHIFTRB		k3, k2, 1							; new borrows shift right to align which lane to add borrow from
                KTESTB			k3, k3								; no new borrows? exit
				JZ				@@saveexit							; If, after alignment shift, there are no borrows, save and exit

; anything else, and for as long as these subtractions cause borrows, subtract one from each borrowed from SIMD lane
				VPBROADCASTQ	ZMM28 {k3}{z}, R9					; Apply borrow-ins only where needed
				VPSUBQ			ZMM29 {k3}, ZMM29, ZMM28
				VPCMPUQ			k1 { k3 }, ZMM29, ZMM28, CPGT		; compute new mask of borrows
				JMP				@B
@@saveexit:
                KMOVB			RAX, k7								; Move final borrows done to RAX
                AND				RAX, retcode_one					; bit 0 (MSB borrow-out)
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

				LEA				RAX, [ retcode_zero ]
				CMOVC			EAX, ret_one
				RET

	ENDIF
				Leaf_End		sub_uT64, ui512						; end of proc, end of section

	END