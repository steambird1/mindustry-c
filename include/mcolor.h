/*
 mcolor.h : Pack and unpack color support.
 This is not provided as a standard part of Mindustry-C becaue when I write this,
 we don't have unpack color, and thus it seems useless.
*/

#ifndef _MCOLOR_H
#define _MCOLOR_H

struct colorinfo_t {
    float r;
    float g;
    float b;
    float alpha;
};

typedef double color_t;

colorinfo_t rgba(float r, float g, float b, float a) {
    colorinfo_t result;
    result.r = r; result.g = g; result.b = b; result.alpha = a;
    return result;
}

color_t packcolorw(float r, float g, float b, float a) {
    asm(":varStorage __r r");
    asm(":varStorage __g g");
    asm(":varStorage __b b");
    asm(":varStorage __a a");
    asm("packcolor __pc_result __r __g __b __a");
    return asm(":varRead __pc_result");
}

color_t packcolor(colorinfo_t colinfo) {
    return packcolorw(colinfo.r, colinfo.g, colinfo.b, colinfo.alpha);
}

colorinfo_t unpackcolor(color_t col) {
    asm(":varStorage __col col");
    asm("unpackcolor __r __g __b __a __col");
    colorinfo_t result;
    result.r = asm(":varRead __r");
    result.g = asm(":varRead __g");
    result.b = asm(":varRead __b");
    result.alpha = asm(":varRead __a");
    return result;
}

#endif