#!/bin/bash

# Define source and destination directories
SOURCE_DIR="../../../lib/X86/X86-64-semantics"
DEST_DIR="../../../lib/AArch64/auto-generated/src"

# Process files with direct name mapping
DIRECT_FILES=(
    "ADDSD.c"
    "BSF_BSR.c"
    "CBW_CWDE_CDQE.c"
    "CMOV.c"
    "CVTSD2SS.c"
    "CVTSI2SD.c"
    "CVTSI2SS.c"
    "CVTSS2SD.c"
    "CVTTSD2SI.c"
    "CVTTSS2SI.c"
    "CWD_CDQ_CQO.c"
    "DIV.c"
    "IDIV.c"
    "IMUL.c"
    "MOVAPS.c"
    "MOVD.c"
    "MOVQ.c"
    "MOVSD.c"
    "MOVSX.c"
    "MOVZX.c"
    "MUL.c"
    "MULSD.c"
    "NEG.c"
    "SETCC.c"
    "DIVSD.c"
    "ADDSS.c"
    "DIVSS.c"
    "MOVSS.c"
    "MOVAPD.c"
    "MULSS.c"
    "SUBSD.c"
    "SUBSS.c"
    "XORPD.c"
    "XORPS.c"
    "PXOR.c"
    "ANDPD.c"
    "ANDPS.c"
    "PAND.c"
)

# Process files with special name mapping
SPECIAL_MAPPINGS=(
    "GenericArithmeticAndLogicInstructions.c:ArithmeticAndLogicInstructions.c"
    "GenericConditionalInstructions.c:ConditionalInstructions.c"
    "GenericDataProcessingInstruction.c:DataProcessingInstructions.c"
    "GenericInstructions.c:UncategorizedInstructions.c"
)

echo "Processing files with direct name mapping..."
for file in "${DIRECT_FILES[@]}"; do
    echo "Processing $file"
    cargo run --release -- "$SOURCE_DIR/$file" > "$DEST_DIR/$file"
done

echo "Processing files with special name mapping..."
for mapping in "${SPECIAL_MAPPINGS[@]}"; do
    SOURCE_FILE=$(echo $mapping | cut -d ":" -f 1)
    DEST_FILE=$(echo $mapping | cut -d ":" -f 2)
    echo "Processing $SOURCE_FILE -> $DEST_FILE"
    cargo run --release -- "$SOURCE_DIR/$SOURCE_FILE" > "$DEST_DIR/$DEST_FILE"
done

echo "All files processed successfully!"