!function(r){"use strict";function n(r,n,t){return t.a=r,t.f=n,t}function e(t){return n(2,t,function(n){return function(r){return t(n,r)}})}function t(e){return n(3,e,function(t){return function(n){return function(r){return e(t,n,r)}}})}function u(u){return n(4,u,function(e){return function(t){return function(n){return function(r){return u(e,t,n,r)}}}})}function a(a){return n(5,a,function(u){return function(e){return function(t){return function(n){return function(r){return a(u,e,t,n,r)}}}}})}function i(i){return n(6,i,function(a){return function(u){return function(e){return function(t){return function(n){return function(r){return i(a,u,e,t,n,r)}}}}}})}function o(o){return n(7,o,function(i){return function(a){return function(u){return function(e){return function(t){return function(n){return function(r){return o(i,a,u,e,t,n,r)}}}}}}})}function b(r,n,t){return 2===r.a?r.f(n,t):r(n)(t)}function s(r,n,t,e){return 3===r.a?r.f(n,t,e):r(n)(t)(e)}function d(r,n,t,e,u){return 4===r.a?r.f(n,t,e,u):r(n)(t)(e)(u)}function l(r,n,t,e,u,a){return 5===r.a?r.f(n,t,e,u,a):r(n)(t)(e)(u)(a)}function c(r,n,t,e,u,a,i){return 6===r.a?r.f(n,t,e,u,a,i):r(n)(t)(e)(u)(a)(i)}function v(r,n,t,e,u,a,i,o){return 7===r.a?r.f(n,t,e,u,a,i,o):r(n)(t)(e)(u)(a)(i)(o)}var h={$:0};function $(r,n){return{$:1,a:r,b:n}}var f=e($);function g(r){for(var n=h,t=r.length;t--;)n=$(r[t],n);return n}function p(r){for(var n=[];r.b;r=r.b)n.push(r.a);return n}function m(r,n){for(var t,e=[],u=y(r,n,0,e);u&&(t=e.pop());u=y(t.a,t.b,0,e));return u}function y(r,n,t,e){if(100<t)return e.push(k(r,n)),!0;if(r===n)return!0;if("object"!=typeof r||null===r||null===n)return"function"==typeof r&&O(5),!1;for(var u in r.$<0&&(r=jn(r),n=jn(n)),r)if(!y(r[u],n[u],t+1,e))return!1;return!0}function w(r,n,t){if("object"!=typeof r)return r===n?0:r<n?-1:1;if(!r.$)return(t=w(r.a,n.a))?t:(t=w(r.b,n.b))?t:w(r.c,n.c);for(;r.b&&n.b&&!(t=w(r.a,n.a));r=r.b,n=n.b);return t||(r.b?1:n.b?-1:0)}var C=e(function(r,n){var t=w(r,n);return t<0?Cn:t?wn:yn}),A=0;function k(r,n){return{a:r,b:n}}function j(r,n,t){return{a:r,b:n,c:t}}function L(r){return r}function N(r,n){var t={};for(var e in r)t[e]=r[e];for(var e in n)t[e]=n[e];return t}function _(r,n){if("string"==typeof r)return r+n;if(!r.b)return n;var t=$(r.a,n);r=r.b;for(var e=t;r.b;r=r.b)e=e.b=$(r.a,n);return t}var x=t(function(r,n,t){for(var e=Array(r),u=0;u<r;u++)e[u]=t(n+u);return e}),E=e(function(r,n){for(var t=Array(r),e=0;e<r&&n.b;e++)t[e]=n.a,n=n.b;return t.length=e,k(t,n)});function O(r){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+r+".md")}var T=e(Math.pow),I=e(function(r,n){var t=n%r;return 0===r?O(11):0<t&&r<0||t<0&&0<r?t+r:t});var S=Math.ceil,z=Math.floor,R=Math.round,F=Math.log;var G=e(function(r,n){return r+n});var H=t(function(r,n,t){for(var e=t.length;e--;){var u=t[e],a=t.charCodeAt(e);a<56320||57343<a||(u=t[--e]+u),n=b(r,L(u),n)}return n}),K=e(function(r,n){return n.split(r)}),M=e(function(r,n){return n.join(r)}),q=t(function(r,n,t){return t.slice(r,n)});var D=e(function(r,n){return-1<n.indexOf(r)}),B=e(function(r,n){return 0==n.indexOf(r)}),P=e(function(r,n){return r.length<=n.length&&n.lastIndexOf(r)==n.length-r.length}),Z=e(function(r,n){var t=r.length;if(t<1)return h;for(var e=0,u=[];-1<(e=n.indexOf(r,e));)u.push(e),e+=t;return g(u)});var V=e(function(r,n){return{$:10,d:r,b:n}});var Y=e(function(r,n){return{$:14,b:n,h:r}});var J=e(function(r,n){return U(r,er(n))});function U(r,n){switch(r.$){case 3:return"boolean"==typeof n?Ct(n):X("a BOOL",n);case 2:return"number"!=typeof n?X("an INT",n):-2147483647<n&&n<2147483647&&(0|n)===n?Ct(n):!isFinite(n)||n%1?X("an INT",n):Ct(n);case 4:return"number"==typeof n?Ct(n):X("a FLOAT",n);case 6:return"string"==typeof n?Ct(n):n instanceof String?Ct(n+""):X("a STRING",n);case 9:return null===n?Ct(r.c):X("null",n);case 5:return Ct(tr(n));case 7:return Array.isArray(n)?W(r.b,n,g):X("a LIST",n);case 8:return Array.isArray(n)?W(r.b,n,Q):X("an ARRAY",n);case 10:var t=r.d;if("object"!=typeof n||null===n||!(t in n))return X("an OBJECT with a field named `"+t+"`",n);var e=U(r.b,n[t]);return et(e)?e:wt(b(kt,t,e.a));case 11:var u=r.e;if(!Array.isArray(n))return X("an ARRAY",n);if(n.length<=u)return X("a LONGER array. Need index "+u+" but only see "+n.length+" entries",n);e=U(r.b,n[u]);return et(e)?e:wt(b(jt,u,e.a));case 12:if("object"!=typeof n||null===n||Array.isArray(n))return X("an OBJECT",n);var a=h;for(var i in n)if(n.hasOwnProperty(i)){e=U(r.b,n[i]);if(!et(e))return wt(b(kt,i,e.a));a=$(k(i,e.a),a)}return Ct(bt(a));case 13:for(var o=r.f,f=r.g,c=0;c<f.length;c++){e=U(f[c],n);if(!et(e))return e;o=o(e.a)}return Ct(o);case 14:e=U(r.b,n);return et(e)?U(r.h(e.a),n):e;case 15:for(var v=h,s=r.g;s.b;s=s.b){e=U(s.a,n);if(et(e))return e;v=$(e.a,v)}return wt(Lt(bt(v)));case 1:return wt(b(At,r.a,tr(n)));case 0:return Ct(r.a)}}function W(r,n,t){for(var e=n.length,u=Array(e),a=0;a<e;a++){var i=U(r,n[a]);if(!et(i))return wt(b(jt,a,i.a));u[a]=i.a}return Ct(t(u))}function Q(n){return b(yt,n.length,function(r){return n[r]})}function X(r,n){return wt(b(At,"Expecting "+r,tr(n)))}function rr(r,n){if(r===n)return!0;if(r.$!==n.$)return!1;switch(r.$){case 0:case 1:return r.a===n.a;case 3:case 2:case 4:case 6:case 5:return!0;case 9:return r.c===n.c;case 7:case 8:case 12:return rr(r.b,n.b);case 10:return r.d===n.d&&rr(r.b,n.b);case 11:return r.e===n.e&&rr(r.b,n.b);case 13:return r.f===n.f&&nr(r.g,n.g);case 14:return r.h===n.h&&rr(r.b,n.b);case 15:return nr(r.g,n.g)}}function nr(r,n){var t=r.length;if(t!==n.length)return!1;for(var e=0;e<t;e++)if(!rr(r[e],n[e]))return!1;return!0}function tr(r){return r}function er(r){return r}tr(null);function ur(r){return{$:0,a:r}}function ar(r){return{$:2,b:r,c:null}}var ir=e(function(r,n){return{$:3,b:r,d:n}});var or=0;function fr(r){var n={$:0,e:or++,f:r,g:null,h:[]};return dr(n),n}function cr(n){return ar(function(r){r(ur(fr(n)))})}function vr(r,n){r.h.push(n),dr(r)}var sr=!1,br=[];function dr(r){if(br.push(r),!sr){for(sr=!0;r=br.shift();)lr(r);sr=!1}}function lr(n){for(;n.f;){var r=n.f.$;if(0===r||1===r){for(;n.g&&n.g.$!==r;)n.g=n.g.i;if(!n.g)return;n.f=n.g.b(n.f.a),n.g=n.g.i}else{if(2===r)return void(n.f.c=n.f.b(function(r){n.f=r,dr(n)}));if(5===r){if(0===n.h.length)return;n.f=n.f.b(n.h.shift())}else n.g={$:3===r?0:1,b:n.f.b,i:n.g},n.f=n.f.d}}}function hr(r,n,t,e,u,a){var i=b(J,r,tr(n?n.flags:void 0));et(i)||O(2);var o={},f=(i=t(i.a)).a,c=a(s,f),v=function(r,n){var t;for(var e in $r){var u=$r[e];u.a&&((t=t||{})[e]=u.a(e,n)),r[e]=gr(u,n)}return t}(o,s);function s(r,n){c(f=(i=b(e,r,f)).a,n),wr(o,i.b,u(f))}return wr(o,i.b,u(f)),v?{ports:v}:{}}var $r={};function gr(r,n){var e={g:n,h:void 0},u=r.c,a=r.d,i=r.e,o=r.f;function f(t){return b(ir,f,{$:5,b:function(r){var n=r.a;return 0===r.$?s(a,e,n,t):i&&o?d(u,e,n.i,n.j,t):s(u,e,i?n.i:n.j,t)}})}return e.h=fr(b(ir,f,r.b))}var pr=e(function(n,t){return ar(function(r){n.g(t),r(ur(A))})});function mr(n){return function(r){return{$:1,k:n,l:r}}}function yr(r){return{$:2,m:r}}function wr(r,n,t){var e={};for(var u in Cr(!0,n,e,null),Cr(!1,t,e,null),r)vr(r[u],{$:"fx",a:e[u]||{i:h,j:h}})}function Cr(r,n,t,e){switch(n.$){case 1:var u=n.k,a=function(r,n,t,e){function u(r){for(var n=t;n;n=n.q)r=n.p(r);return r}return b(r?$r[n].e:$r[n].f,u,e)}(r,u,e,n.l);return void(t[u]=function(r,n,t){return t=t||{i:h,j:h},r?t.i=$(n,t.i):t.j=$(n,t.j),t}(r,a,t[u]));case 2:for(var i=n.m;i.b;i=i.b)Cr(r,i.a,t,e);return;case 3:return void Cr(r,n.o,t,{p:n.n,q:e})}}var Ar=t(function(r,n,t){return n<t.length?55296==(63488&t.charCodeAt(n))?r(L(t.substr(n,2)))?n+2:-1:r(L(t[n]))?"\n"===t[n]?-2:n+1:-1:-1});var kr,jr=e(function(r,n){var t="g";r.au&&(t+="m"),r.ad&&(t+="i");try{return Un(RegExp(n,t))}catch(r){return Wn}}),Lr=u(function(u,r,a,n){var i=0;return n.replace(r,function(r){if(i++>=u)return r;for(var n=arguments.length-3,t=Array(n);0<n;){var e=arguments[n];t[--n]=e?Un(e):Wn}return a(d(iu,r,arguments[arguments.length-2],i,g(t)))})}),Nr="undefined"!=typeof document?document:{};function _r(r,n){r.appendChild(n)}function xr(r){return{$:0,a:r}}var Er=e(function(a,i){return e(function(r,n){for(var t=[],e=0;n.b;n=n.b){var u=n.a;e+=u.b||0,t.push(u)}return e+=t.length,{$:1,c:i,d:Rr(r),e:t,f:a,b:e}})}),Or=Er(void 0);e(function(a,i){return e(function(r,n){for(var t=[],e=0;n.b;n=n.b){var u=n.a;e+=u.b.b||0,t.push(u)}return e+=t.length,{$:2,c:i,d:Rr(r),e:t,f:a,b:e}})})(void 0);var Tr=e(function(r,n){return{$:"a1",n:r,o:n}}),Ir=e(function(r,n){return{$:"a2",n:r,o:n}}),Sr=e(function(r,n){return{$:"a3",n:r,o:n}});var zr;function Rr(r){for(var n={};r.b;r=r.b){var t=r.a,e=t.$,u=t.n,a=t.o;if("a2"!==e){var i=n[e]||(n[e]={});"a3"===e&&"class"===u?Fr(i,u,a):i[u]=a}else"className"===u?Fr(n,u,er(a)):n[u]=er(a)}return n}function Fr(r,n,t){var e=r[n];r[n]=e?e+" "+t:t}function Gr(r,n){var t=r.$;if(5===t)return Gr(r.k||(r.k=r.m()),n);if(0===t)return Nr.createTextNode(r.a);if(4===t){for(var e=r.k,u=r.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var a={j:u,p:n};return(i=Gr(e,a)).elm_event_node_ref=a,i}if(3===t)return Hr(i=r.h(r.g),n,r.d),i;var i=r.f?Nr.createElementNS(r.f,r.c):Nr.createElement(r.c);kr&&"a"==r.c&&i.addEventListener("click",kr(i)),Hr(i,n,r.d);for(var o=r.e,f=0;f<o.length;f++)_r(i,Gr(1===t?o[f]:o[f].b,n));return i}function Hr(r,n,t){for(var e in t){var u=t[e];"a1"===e?Kr(r,u):"a0"===e?Dr(r,n,u):"a3"===e?Mr(r,u):"a4"===e?qr(r,u):("value"!==e||"checked"!==e||r[e]!==u)&&(r[e]=u)}}function Kr(r,n){var t=r.style;for(var e in n)t[e]=n[e]}function Mr(r,n){for(var t in n){var e=n[t];e?r.setAttribute(t,e):r.removeAttribute(t)}}function qr(r,n){for(var t in n){var e=n[t],u=e.f,a=e.o;a?r.setAttributeNS(u,t,a):r.removeAttributeNS(u,t)}}function Dr(r,n,t){var e=r.elmFs||(r.elmFs={});for(var u in t){var a=t[u],i=e[u];if(a){if(i){if(i.q.$===a.$){i.q=a;continue}r.removeEventListener(u,i)}i=Br(n,a),r.addEventListener(u,i,zr&&{passive:ju(a)<2}),e[u]=i}else r.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){zr=!0}}))}catch(r){}function Br(v,r){function s(r){var n=s.q,t=U(n.a,r);if(et(t)){for(var e,u=ju(n),a=t.a,i=u?u<3?a.a:a.p:a,o=1==u?a.b:3==u&&a.aa,f=(o&&r.stopPropagation(),(2==u?a.b:3==u&&a.Y)&&r.preventDefault(),v);e=f.j;){if("function"==typeof e)i=e(i);else for(var c=e.length;c--;)i=e[c](i);f=f.p}f(i,o)}}return s.q=r,s}function Pr(r,n){return r.$==n.$&&rr(r.a,n.a)}function Zr(r,n){var t=[];return Yr(r,n,t,0),t}function Vr(r,n,t,e){var u={$:n,r:t,s:e,t:void 0,u:void 0};return r.push(u),u}function Yr(r,n,t,e){if(r!==n){var u=r.$,a=n.$;if(u!==a){if(1!==u||2!==a)return void Vr(t,0,e,n);n=function(r){for(var n=r.e,t=n.length,e=Array(t),u=0;u<t;u++)e[u]=n[u].b;return{$:1,c:r.c,d:r.d,e:e,f:r.f,b:r.b}}(n),a=1}switch(a){case 5:for(var i=r.l,o=n.l,f=i.length,c=f===o.length;c&&f--;)c=i[f]===o[f];if(c)return void(n.k=r.k);n.k=n.m();var v=[];return Yr(r.k,n.k,v,0),void(0<v.length&&Vr(t,1,e,v));case 4:for(var s=r.j,b=n.j,d=!1,l=r.k;4===l.$;)d=!0,"object"!=typeof s?s=[s,l.j]:s.push(l.j),l=l.k;for(var h=n.k;4===h.$;)d=!0,"object"!=typeof b?b=[b,h.j]:b.push(h.j),h=h.k;return d&&s.length!==b.length?void Vr(t,0,e,n):((d?function(r,n){for(var t=0;t<r.length;t++)if(r[t]!==n[t])return!1;return!0}(s,b):s===b)||Vr(t,2,e,b),void Yr(l,h,t,e+1));case 0:return void(r.a!==n.a&&Vr(t,3,e,n.a));case 1:return void Jr(r,n,t,e,Wr);case 2:return void Jr(r,n,t,e,Qr);case 3:if(r.h!==n.h)return void Vr(t,0,e,n);var $=Ur(r.d,n.d);$&&Vr(t,4,e,$);var g=n.i(r.g,n.g);return void(g&&Vr(t,5,e,g))}}}function Jr(r,n,t,e,u){if(r.c===n.c&&r.f===n.f){var a=Ur(r.d,n.d);a&&Vr(t,4,e,a),u(r,n,t,e)}else Vr(t,0,e,n)}function Ur(r,n,t){var e;for(var u in r)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in n){var a=r[u],i=n[u];a===i&&"value"!==u&&"checked"!==u||"a0"===t&&Pr(a,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:r[u].f,o:void 0}:"string"==typeof r[u]?"":null;else{var o=Ur(r[u],n[u]||{},u);o&&((e=e||{})[u]=o)}for(var f in n)f in r||((e=e||{})[f]=n[f]);return e}function Wr(r,n,t,e){var u=r.e,a=n.e,i=u.length,o=a.length;o<i?Vr(t,6,e,{v:o,i:i-o}):i<o&&Vr(t,7,e,{v:i,e:a});for(var f=i<o?i:o,c=0;c<f;c++){var v=u[c];Yr(v,a[c],t,++e),e+=v.b||0}}function Qr(r,n,t,e){for(var u=[],a={},i=[],o=r.e,f=n.e,c=o.length,v=f.length,s=0,b=0,d=e;s<c&&b<v;){var l=(L=o[s]).a,h=(N=f[b]).a,$=L.b,g=N.b;if(l!==h){var p=o[s+1],m=f[b+1];if(p)var y=p.a,w=p.b,C=h===y;if(m)var A=m.a,k=m.b,j=l===A;if(j&&C)Yr($,k,u,++d),rn(a,u,l,g,b,i),d+=$.b||0,nn(a,u,l,w,++d),d+=w.b||0,s+=2,b+=2;else if(j)d++,rn(a,u,h,g,b,i),Yr($,k,u,d),d+=$.b||0,s+=1,b+=2;else if(C)nn(a,u,l,$,++d),d+=$.b||0,Yr(w,g,u,++d),d+=w.b||0,s+=2,b+=1;else{if(!p||y!==A)break;nn(a,u,l,$,++d),rn(a,u,h,g,b,i),d+=$.b||0,Yr(w,k,u,++d),d+=w.b||0,s+=2,b+=2}}else Yr($,g,u,++d),d+=$.b||0,s++,b++}for(;s<c;){var L;nn(a,u,(L=o[s]).a,$=L.b,++d),d+=$.b||0,s++}for(;b<v;){var N,_=_||[];rn(a,u,(N=f[b]).a,N.b,void 0,_),b++}(0<u.length||0<i.length||_)&&Vr(t,8,e,{w:u,x:i,y:_})}var Xr="_elmW6BL";function rn(r,n,t,e,u,a){var i=r[t];if(!i)return a.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(r[t]=i);if(1===i.c){a.push({r:u,A:i}),i.c=2;var o=[];return Yr(i.z,e,o,i.r),i.r=u,void(i.s.s={w:o,A:i})}rn(r,n,t+Xr,e,u,a)}function nn(r,n,t,e,u){var a=r[t];if(a){if(0===a.c){a.c=2;var i=[];return Yr(e,a.z,i,u),void Vr(n,9,u,{w:i,A:a})}nn(r,n,t+Xr,e,u)}else{var o=Vr(n,9,u,void 0);r[t]={c:1,z:e,r:u,s:o}}}function tn(r,n,t,e){!function r(n,t,e,u,a,i,o){var f=e[u];var c=f.r;for(;c===a;){var v=f.$;if(1===v)tn(n,t.k,f.s,o);else if(8===v){f.t=n,f.u=o;var s=f.s.w;0<s.length&&r(n,t,s,0,a,i,o)}else if(9===v){f.t=n,f.u=o;var b=f.s;if(b){b.A.s=n;var s=b.w;0<s.length&&r(n,t,s,0,a,i,o)}}else f.t=n,f.u=o;if(!(f=e[++u])||(c=f.r)>i)return u}var d=t.$;if(4===d){for(var l=t.k;4===l.$;)l=l.k;return r(n,l,e,u,a+1,i,n.elm_event_node_ref)}var h=t.e;var $=n.childNodes;for(var g=0;g<h.length;g++){var p=1===d?h[g]:h[g].b,m=++a+(p.b||0);if(a<=c&&c<=m&&(u=r($[g],p,e,u,a,m,o),!(f=e[u])||(c=f.r)>i))return u;a=m}return u}(r,n,t,0,0,n.b,e)}function en(r,n,t,e){return 0===t.length?r:(tn(r,n,t,e),un(r,t))}function un(r,n){for(var t=0;t<n.length;t++){var e=n[t],u=e.t,a=an(u,e);u===r&&(r=a)}return r}function an(r,n){switch(n.$){case 0:return function(r,n,t){var e=r.parentNode,u=Gr(n,t);u.elm_event_node_ref||(u.elm_event_node_ref=r.elm_event_node_ref);e&&u!==r&&e.replaceChild(u,r);return u}(r,n.s,n.u);case 4:return Hr(r,n.u,n.s),r;case 3:return r.replaceData(0,r.length,n.s),r;case 1:return un(r,n.s);case 2:return r.elm_event_node_ref?r.elm_event_node_ref.j=n.s:r.elm_event_node_ref={j:n.s,p:n.u},r;case 6:for(var t=n.s,e=0;e<t.i;e++)r.removeChild(r.childNodes[t.v]);return r;case 7:for(var u=(t=n.s).e,a=r.childNodes[e=t.v];e<u.length;e++)r.insertBefore(Gr(u[e],n.u),a);return r;case 9:if(!(t=n.s))return r.parentNode.removeChild(r),r;var i=t.A;return void 0!==i.r&&r.parentNode.removeChild(r),i.s=un(r,t.w),r;case 8:return function(r,n){var t=n.s,e=function(r,n){if(!r)return;for(var t=Nr.createDocumentFragment(),e=0;e<r.length;e++){var u=r[e],a=u.A;_r(t,2===a.c?a.s:Gr(a.z,n.u))}return t}(t.y,n);r=un(r,t.w);for(var u=t.x,a=0;a<u.length;a++){var i=u[a],o=i.A,f=2===o.c?o.s:Gr(o.z,n.u);r.insertBefore(f,r.childNodes[i.r])}e&&_r(r,e);return r}(r,n);case 5:return n.s(r);default:O(10)}}function on(r){if(3===r.nodeType)return xr(r.textContent);if(1!==r.nodeType)return xr("");for(var n=h,t=r.attributes,e=t.length;e--;){var u=t[e];n=$(b(Sr,u.name,u.value),n)}var a=r.tagName.toLowerCase(),i=h,o=r.childNodes;for(e=o.length;e--;)i=$(on(o[e]),i);return s(Or,a,n,i)}var fn=u(function(n,r,t,e){return hr(r,e,n.a$,n.bb,n.a9,function(u,r){var a=n.L&&n.L(u),i=n.bd,o=Nr.title,f=Nr.body,c=on(f);return vn(r,function(r){kr=a;var n=i(r),t=Or("body")(h)(n.aS),e=Zr(c,t);f=en(f,c,e,u),c=t,kr=0,o!==n.ba&&(Nr.title=o=n.ba)})})}),cn="undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(r){setTimeout(r,1e3/60)};function vn(t,e){e(t);var u=0;function a(){u=1===u?0:(cn(a),e(t),1)}return function(r,n){t=r,n?(e(t),2===u&&(u=1)):(0===u&&cn(a),u=2)}}function sn(){return Ru(Nr.location.href).a||O(1)}var bn=e(function(r,n){return b(Au,du,ar(function(){history.pushState({},"",n),r()}))}),dn={addEventListener:function(){},removeEventListener:function(){}},ln=("undefined"!=typeof document&&document,"undefined"!=typeof window?window:dn);var hn,$n,gn,pn={$:0},mn=u(function(r,n,t,e){return{$:1,a:r,b:n,c:t,d:e}}),yn=1,wn=2,Cn=0,An=t(function(r,n,t){for(;;){if(-2===t.$)return n;var e=t.d,u=r,a=s(r,t.b,t.c,s(An,r,n,t.e));r=u,n=a,t=e}}),kn=f,jn=function(r){return s(An,t(function(r,n,t){return b(kn,k(r,n),t)}),h,r)},Ln=z,Nn=u(function(r,n,t,e){return d(mn,r-6.283185307179586*Ln(r/6.283185307179586),n,t,e)}),_n=I,xn=e(function(r,n){var t=Ln(r);return b(_n,n,t)+r-t}),En=function(r){return-r},On=function(r){return r<0?-r:r},Tn=function(r){return 3.141592653589793*r/180},In=e(function(r,n){return 0<w(r,n)?r:n}),Sn=e(function(r,n){return w(r,n)<0?r:n}),zn=t(function(r,n,t){var e=r/255,u=n/255,a=t/255,i=b(In,b(In,e,u),a),o=b(Sn,b(Sn,e,u),a),f=i-o,c=(i+o)/2,v=c?f/(1-On(2*c-1)):0;return j(Tn(60)*(m(i,e)?b(xn,(u-a)/f,6):m(i,u)?(a-e)/f+2:(e-u)/f+4),v,c)}),Rn=t(function(r,n,t){var e=r/Tn(60),u=(1-On(2*t-1))*n,a=t-u/2,i=u*(1-On(b(xn,e,2)-1)),o=e<0?j(0,0,0):e<1?j(u,i,0):e<2?j(i,u,0):e<3?j(0,u,i):e<4?j(0,i,u):e<5?j(i,0,u):e<6?j(u,0,i):j(0,0,0);return j(o.a+a,o.b+a,o.c+a)}),Fn=R,Gn=function(r){if(r.$){u=r.d;var n=s(Rn,r.a,r.b,r.c);t=n.a,e=n.b;return{R:u,G:Fn(255*n.c),I:Fn(255*e),K:Fn(255*t)}}var t,e,u;return{R:u=r.d,G:r.c,I:e=r.b,K:t=r.a}},Hn=T,Kn=function(r){var n=function(r){var n=r/255;return.03928<n?b(Hn,(n+.055)/1.055,2.4):n/12.92},t=Gn(r),e=t.I,u=t.G;return.2126*n(t.K)+.7152*n(e)+.0722*n(u)},Mn=e(function(r,n){var t=(Kn(r)+.05)/(Kn(n)+.05);return t<1?1/t:t}),qn=u(function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}}),Dn=t(function(r,n,t){return d(qn,r,n,t,1)}),Bn=function(r){return r},Pn=e(function(r,n){return{$:0,a:r,b:n}}),Zn=function(r){var n=r.b;return b(Pn,1664525*r.a+n>>>0,n)},Vn=function(r){var n=r.a,t=277803737*(n^n>>>4+(n>>>28));return(t>>>22^t)>>>0},Yn=b(e(function(t,i){return function(r){var n=w(t,i)<0?k(t,i):k(i,t),e=n.a,u=n.b-e+1;if(u-1&u){var a=(-u>>>0)%u>>>0;return function(r){for(;;){var n=Vn(r),t=Zn(r);if(0<=w(n,a))return k(n%u+e,t);r=t}}(r)}return k(((u-1&Vn(r))>>>0)+e,Zn(r))}}),0,255),Jn=d(u(function(o,r,n,t){var f=r,c=n,v=t;return function(r){var n=f(r),t=n.a,e=c(n.b),u=e.a,a=v(e.b),i=a.b;return k(s(o,t,u,a.a),i)}}),Dn,Yn,Yn,Yn),Un=function(r){return{$:0,a:r}},Wn={$:1},Qn=e(function(r,n){return r(n)}),Xn=u(function(r,n,t,e){for(;;){if(!n)return k(Wn,t);var u=b(Qn,Jn,t),a=u.a,i=u.b;if(-1<w(b(Mn,e,a),r))return k(Un(a),i);r=r,n=n-1,t=i,e=e}}),rt=e(function(r,n){return n.$?r:n.a}),nt=function(r){var n=Zn(b(Pn,0,1013904223));return Zn(b(Pn,n.a+r>>>0,n.b))},tt=function(r){var n=nt(function(r){var n=Gn(r);return 1e6*n.K+1e3*n.I+n.G}(r)),t=function(r){if(1===r.$){var n=r.b,t=r.c,e=r.d;return d(Nn,r.a+Tn(180),n,t,e)}e=r.d;var u=s(zn,r.a,r.b,r.c);n=u.b,t=u.c;return d(Nn,u.a+Tn(180),n,t,e)}(r);return b(rt,t,d(Xn,3,50,n,r).a)},et=function(r){return!r.$},ut=u(function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}}),at=S,it=e(function(r,n){return F(n)/F(r)}),ot=at(b(it,2,32)),ft=[],ct=d(ut,0,ot,ft,ft),vt=E,st=t(function(r,n,t){for(;;){if(!t.b)return n;var e=t.b,u=r,a=b(r,t.a,n);r=u,n=a,t=e}}),bt=function(r){return s(st,kn,h,r)},dt=e(function(r,n){for(;;){var t=b(vt,32,r),e=t.b,u=b(kn,{$:0,a:t.a},n);if(!e.b)return bt(u);r=e,n=u}}),lt=e(function(r,n){for(;;){var t=at(n/32);if(1===t)return b(vt,32,r).a;r=b(dt,r,h),n=t}}),ht=e(function(r,n){return r(n)}),$t=function(r){return r.length},gt=e(function(r,n){if(n.e){var t=32*n.e,e=Ln(b(it,32,t-1)),u=r?bt(n.h):n.h,a=b(lt,u,n.e);return d(ut,$t(n.g)+t,b(In,5,e*ot),a,n.g)}return d(ut,$t(n.g),ot,ft,n.g)}),pt=x,mt=a(function(r,n,t,e,u){for(;;){if(n<0)return b(gt,!1,{h:e,e:t/32|0,g:u});var a={$:1,a:s(pt,32,n,r)};r=r,n=n-32,t=t,e=b(kn,a,e),u=u}}),yt=e(function(r,n){if(0<r){var t=r%32;return l(mt,n,r-t-32,r,h,s(pt,t,r-t,n))}return ct}),wt=function(r){return{$:1,a:r}},Ct=function(r){return{$:0,a:r}},At=e(function(r,n){return{$:3,a:r,b:n}}),kt=e(function(r,n){return{$:0,a:r,b:n}}),jt=e(function(r,n){return{$:1,a:r,b:n}}),Lt=function(r){return{$:2,a:r}},Nt=function(r){var n=r.charCodeAt(0);return n<55296||56319<n?n:1024*(n-55296)+r.charCodeAt(1)-56320+65536},_t=function(r){return s(st,e(function(r,n){return n+1}),0,r)},xt=function(r){return r+""},Et=e(function(r,n){return b(M,r,p(n))}),Ot=e(function(r,n){return g(b(K,r,n))}),Tt=yr(h),It=e(function(r,n){if(1===r.$)return k(N(n,{u:pn}),Tt);switch(r.a.$){case 2:return k(N(n,{u:pn}),Tt);case 0:var t=r.a.a;return k(N(n,{u:(a={F:t,N:tt(t)},{$:1,a:a})}),Tt);default:var e=function(r){var n=b(Qn,Jn,r),t=n.a,e=n.b;return k({F:t,N:tt(t)},e)}(n.S),u=e.b;return k(N(n,{u:function(r){return{$:2,a:r}}(e.a),S:u}),Tt)}var a}),St=function(r){var n=Nt(r);return 48<=n&&n<=57||65<=n&&n<=70||97<=n&&n<=102},zt=function(r){return r.toLowerCase()},Rt=e(function(r,n){return{$:1,a:r,b:n}}),Ft=t(function(r,n,t){return{$:0,a:r,b:n,c:t}}),Gt=e(function(i,r){var o=r;return function(r){var n=o(r);if(1===n.$)return b(Rt,n.a,n.b);var t=n.a,e=n.c,u=i(n.b)(e);if(1===u.$){var a=u.a;return b(Rt,t||a,u.b)}a=u.a;return s(Ft,t||a,u.b,u.c)}}),Ht={$:11},Kt=e(function(r,n){return{$:1,a:r,b:n}}),Mt=u(function(r,n,t,e){return{af:n,aU:e,az:t,aH:r}}),qt={$:0},Dt=e(function(r,n){return b(Kt,qt,d(Mt,r.aH,r.af,n,r.c))}),Bt=Ar,Pt=e(function(t,e){return function(r){var n=s(Bt,t,r.b,r.a);return m(n,-1)?b(Rt,!1,b(Dt,r,e)):m(n,-2)?s(Ft,!0,0,{af:1,c:r.c,d:r.d,b:r.b+1,aH:r.aH+1,a:r.a}):s(Ft,!0,0,{af:r.af+1,c:r.c,d:r.d,b:n,aH:r.aH,a:r.a})}}),Zt=function(r){return b(Pt,r,Ht)},Vt=e(function(r){return r}),Yt=q,Jt=e(function(u,r){var a=r;return function(r){var n=a(r);if(1===n.$)return b(Rt,n.a,n.b);var t=n.b,e=n.c;return s(Ft,n.a,b(u,s(Yt,r.b,e.b,r.a),t),e)}}),Ut=function(r){return b(Jt,Vt,r)},Wt=t(function(o,r,n){var f=r,c=n;return function(r){var n=f(r);if(1===n.$)return b(Rt,n.a,n.b);var t=n.a,e=n.b,u=c(n.c);if(1===u.$){var a=u.a;return b(Rt,t||a,u.b)}a=u.a;var i=u.c;return s(Ft,t||a,b(o,e,u.b),i)}}),Qt=e(function(r,n){return s(Wt,Vt,r,n)}),Xt=function(r){return n={$:12,a:r},function(r){return b(Rt,!1,b(Dt,r,n))};var n},re=function(n){return function(r){return s(Ft,!1,n,r)}},ne=e(function(r,n){return n.$?wt(n.a):Ct(r(n.a))}),te=e(function(r,n){return n.$?wt(r(n.a)):Ct(n.a)}),ee=function(r){return""===r},ue=function(r){return r.length},ae=B,ie=H,oe=function(r){return s(ie,kn,h,r)},fe=G,ce=function(r){return b(fe,r,"")},ve=t(function(r,n,t){r:for(;;){if(!n.b)return Ct(t);var e=n.a,u=n.b;switch(e){case"0":r=a=r-1,n=i=u,t=o=t;continue r;case"1":var a=r-1,i=u,o=t+b(Hn,16,r);r=a,n=i,t=o;continue r;case"2":a=r-1,i=u,o=t+2*b(Hn,16,r);r=a,n=i,t=o;continue r;case"3":a=r-1,i=u,o=t+3*b(Hn,16,r);r=a,n=i,t=o;continue r;case"4":a=r-1,i=u,o=t+4*b(Hn,16,r);r=a,n=i,t=o;continue r;case"5":a=r-1,i=u,o=t+5*b(Hn,16,r);r=a,n=i,t=o;continue r;case"6":a=r-1,i=u,o=t+6*b(Hn,16,r);r=a,n=i,t=o;continue r;case"7":a=r-1,i=u,o=t+7*b(Hn,16,r);r=a,n=i,t=o;continue r;case"8":a=r-1,i=u,o=t+8*b(Hn,16,r);r=a,n=i,t=o;continue r;case"9":a=r-1,i=u,o=t+9*b(Hn,16,r);r=a,n=i,t=o;continue r;case"a":a=r-1,i=u,o=t+10*b(Hn,16,r);r=a,n=i,t=o;continue r;case"b":a=r-1,i=u,o=t+11*b(Hn,16,r);r=a,n=i,t=o;continue r;case"c":a=r-1,i=u,o=t+12*b(Hn,16,r);r=a,n=i,t=o;continue r;case"d":a=r-1,i=u,o=t+13*b(Hn,16,r);r=a,n=i,t=o;continue r;case"e":a=r-1,i=u,o=t+14*b(Hn,16,r);r=a,n=i,t=o;continue r;case"f":a=r-1,i=u,o=t+15*b(Hn,16,r);r=a,n=i,t=o;continue r;default:return wt(ce(e)+" is not a valid hexadecimal character.")}}}),se=function(n){if(ee(n))return wt("Empty strings are not valid hexadecimal strings.");var r=function(){if(b(ae,"-",n)){var r=b(rt,h,function(r){if(r.b)return Un(r.b);return Wn}(oe(n)));return b(ne,En,s(ve,_t(r)-1,r,0))}return s(ve,ue(n)-1,oe(n),0)}();return b(te,function(r){return b(Et," ",g(['"'+n+'"',"is not a valid hexadecimal string because",r]))},r)},be=b(Gt,function(r){var n=se(zt(r));return n.$?Xt(n.a):re(n.a)},Ut(b(Qt,b(Qt,re(0),Zt(St)),Zt(St)))),de=(hn={$:10},function(r){return m(ue(r.a),r.b)?s(Ft,!1,0,r):b(Rt,!1,b(Dt,r,hn))}),le=e(function(r,n){return s(Wt,ht,r,n)}),he=b(le,b(le,b(le,re(Dn),be),be),b(Qt,be,de)),$e=t(function(r,n,t){return n(r(t))}),ge=u(function(r,n,t,e){if(e.b){var u=e.a,a=e.b;if(a.b){var i=a.a,o=a.b;if(o.b){var f=o.a,c=o.b;if(c.b){var v=c.b;return b(r,u,b(r,i,b(r,f,b(r,c.a,500<t?s(st,r,n,bt(v)):d(ge,r,n,t+1,v)))))}return b(r,u,b(r,i,b(r,f,n)))}return b(r,u,b(r,i,n))}return b(r,u,n)}return n}),pe=t(function(r,n,t){return d(ge,r,n,0,t)}),me=e(function(t,r){return s(pe,e(function(r,n){return b(kn,t(r),n)}),h,r)}),ye=t(function(r,n,t){return{af:n,az:t,aH:r}}),we=function(r){return s(ye,r.aH,r.af,r.az)},Ce=e(function(r,n){r:for(;;)switch(r.$){case 0:return n;case 1:var t=r.b;r=r.a,n=b(kn,t,n);continue r;default:var e=r.b;r=r.a,n=b(Ce,e,n);continue r}}),Ae=e(function(r,n){var t=r({af:1,c:h,d:1,b:0,aH:1,a:n});return t.$?wt(b(Ce,t.b,h)):Ct(t.b)}),ke=b($e,e(function(r,n){var t=b(Ae,r,n);return t.$?wt(b(me,we,t.a)):Ct(t.a)})(he),te(function(){return"TODO deadEndsToString"})),je=a(function(r,n,t,e,u){return{s:e,v:t,r:n,n:u,w:r}}),Le=e(function(r,v){return function(r){var n=r.w,t=r.r,e=r.v,u=r.s,a=r.n;if(t.b){var i=t.a,o=t.b,f=v(i);if(f.$)return h;var c=f.a;return g([l(je,b(kn,i,n),o,e,u,a(c))])}return h}}),Ne=b(Le,"COLOR",function(r){return(n=ke(r)).$?Wn:Un(n.a);var n}),_e=function(r){return{$:2,a:r}},xe={$:1},Ee=function(r){return{$:0,a:r}},Oe=e(function(r,n){return l(je,n.w,n.r,n.v,n.s,r(n.n))}),Te=e(function(a,r){var i=r;return function(r){var n=r.w,t=r.r,e=r.v,u=r.s;return b(me,Oe(r.n),i(l(je,n,t,e,u,a)))}}),Ie=e(function(r,n){return n.b?s(pe,kn,n,r):r}),Se=e(function(r,n){return s(pe,Ie,h,b(me,r,n))}),ze=function(r){return r.b&&(""!==r.a||r.b.b)?b(kn,r.a,ze(r.b)):h},Re={$:-2},Fe=Re,Ge=C,He=e(function(r,n){r:for(;;){if(-2===n.$)return Wn;var t=n.c,e=n.d,u=n.e;switch(b(Ge,r,n.b)){case 0:r=r,n=e;continue r;case 1:return Un(t);default:r=r,n=u;continue r}}}),Ke=a(function(r,n,t,e,u){return{$:-1,a:r,b:n,c:t,d:e,e:u}}),Me=a(function(r,n,t,e,u){if(-1!==u.$||u.a){if(-1!==e.$||e.a||-1!==e.d.$||e.d.a)return l(Ke,r,n,t,e,u);var a=e.d;v=e.e;return l(Ke,0,e.b,e.c,l(Ke,1,a.b,a.c,a.d,a.e),l(Ke,1,n,t,v,u))}var i=u.b,o=u.c,f=u.d,c=u.e;if(-1!==e.$||e.a)return l(Ke,r,i,o,l(Ke,0,n,t,e,f),c);var v;return l(Ke,0,n,t,l(Ke,1,e.b,e.c,e.d,v=e.e),l(Ke,1,i,o,f,c))}),qe=t(function(r,n,t){if(-2===t.$)return l(Ke,0,r,n,Re,Re);var e=t.a,u=t.b,a=t.c,i=t.d,o=t.e;switch(b(Ge,r,u)){case 0:return l(Me,e,u,a,s(qe,r,n,i),o);case 1:return l(Ke,e,u,n,i,o);default:return l(Me,e,u,a,i,s(qe,r,n,o))}}),De=t(function(r,n,t){var e=s(qe,r,n,t);if(-1!==e.$||e.a)return e;return l(Ke,1,e.b,e.c,e.d,e.e)}),Be=function(r){if(-1!==r.$||-1!==r.d.$||-1!==r.e.$)return r;if(-1!==r.e.d.$||r.e.d.a){var n=r.d,t=r.e;i=t.b,o=t.c,e=t.d,v=t.e;return l(Ke,1,r.b,r.c,l(Ke,0,n.b,n.c,n.d,n.e),l(Ke,0,i,o,e,v))}var e,u=r.d,a=r.e,i=a.b,o=a.c,f=(e=a.d,e.d),c=e.e,v=a.e;return l(Ke,0,e.b,e.c,l(Ke,1,r.b,r.c,l(Ke,0,u.b,u.c,u.d,u.e),f),l(Ke,1,i,o,c,v))},Pe=function(r){if(-1!==r.$||-1!==r.d.$||-1!==r.e.$)return r;if(-1!==r.d.d.$||r.d.d.a){var n=r.d,t=n.d,e=r.e;v=e.b,s=e.c,b=e.d,d=e.e;return l(Ke,1,u=r.b,a=r.c,l(Ke,0,n.b,n.c,t,f=n.e),l(Ke,0,v,s,b,d))}var u=r.b,a=r.c,i=r.d,o=i.d,f=i.e,c=r.e,v=c.b,s=c.c,b=c.d,d=c.e;return l(Ke,0,i.b,i.c,l(Ke,1,o.b,o.c,o.d,o.e),l(Ke,1,u,a,f,l(Ke,0,v,s,b,d)))},Ze=o(function(r,n,t,e,u,a,i){if(-1!==a.$||a.a){r:for(;-1===i.$&&1===i.a;){if(-1===i.d.$){if(1!==i.d.a)break r;return Pe(n)}return Pe(n)}return n}return l(Ke,t,a.b,a.c,a.d,l(Ke,0,e,u,a.e,i))}),Ve=function(r){if(-1!==r.$||-1!==r.d.$)return Re;var n=r.a,t=r.b,e=r.c,u=r.d,a=u.d,i=r.e;if(1!==u.a)return l(Ke,n,t,e,Ve(u),i);if(-1!==a.$||a.a){var o=Be(r);if(-1!==o.$)return Re;var f=o.e;return l(Me,o.a,o.b,o.c,Ve(o.d),f)}return l(Ke,n,t,e,Ve(u),i)},Ye=e(function(r,n){if(-2===n.$)return Re;var t=n.a,e=n.b,u=n.c,a=n.d,i=n.e;if(w(r,e)<0){if(-1!==a.$||1!==a.a)return l(Ke,t,e,u,b(Ye,r,a),i);var o=a.d;if(-1!==o.$||o.a){var f=Be(n);if(-1!==f.$)return Re;var c=f.e;return l(Me,f.a,f.b,f.c,b(Ye,r,f.d),c)}return l(Ke,t,e,u,b(Ye,r,a),i)}return b(Je,r,v(Ze,r,n,t,e,u,a,i))}),Je=e(function(r,n){if(-1!==n.$)return Re;var t=n.a,e=n.b,u=n.c,a=n.d,i=n.e;if(m(r,e)){var o=function(r){for(;;){if(-1!==r.$||-1!==r.d.$)return r;r=r.d}}(i);return-1!==o.$?Re:l(Me,t,o.b,o.c,a,Ve(i))}return l(Me,t,e,u,a,b(Ye,r,i))}),Ue=e(function(r,n){var t=b(Ye,r,n);if(-1!==t.$||t.a)return t;return l(Ke,1,t.b,t.c,t.d,t.e)}),We=t(function(r,n,t){var e=n(b(He,r,t));return e.$?b(Ue,r,t):s(De,r,e.a,t)}),Qe=function(r){try{return Un(decodeURIComponent(r))}catch(r){return Wn}},Xe=e(function(r,n){return Un(1===n.$?g([r]):b(kn,r,n.a))}),ru=e(function(r,n){var t=b(Ot,"=",r);if(t.b&&t.b.b&&!t.b.b.b){var e=t.b.a,u=Qe(t.a);if(1===u.$)return n;var a=u.a,i=Qe(e);return 1===i.$?n:s(We,a,Xe(i.a),n)}return n}),nu=e(function(r,n){var t;return function(r){r:for(;;){if(r.b){var n=r.a,t=n.r;if(t.b){if(""!==t.a||t.b.b){r=r.b;continue r}return Un(n.n)}return Un(n.n)}return Wn}}(r(l(je,h,function(r){var n=b(Ot,"/",r);return ze(n.b&&""===n.a?n.b:n)}(n.aw),1===(t=n.aD).$?Fe:s(pe,ru,Fe,b(Ot,"&",t.a)),n.al,Bn)))}),tu=function(f){return function(r){var n=r.w,t=r.r,e=r.v,u=r.s,a=r.n;if(t.b){var i=t.a,o=t.b;return m(i,f)?g([l(je,b(kn,i,n),o,e,u,a)]):h}return h}},eu=e(function(r,n){var t=r,e=n;return function(r){return b(Se,e,t(r))}}),uu=b(Le,"STRING",Un),au=nu(($n=g([b(Te,xe,function(r){return g([r])}),b(Te,Ee,b(eu,tu("colors"),Ne)),b(Te,_e,b(eu,tu("colors"),uu))]),function(n){return b(Se,function(r){return r(n)},$n)})),iu=u(function(r,n,t,e){return{a_:n,a1:r,a3:t,a8:e}}),ou=jr,fu=/.^/,cu=Lr(1/0),vu=e(function(r,n){var t=b(rt,fu,b(ou,{ad:!1,au:!1},"^"+r));return N(n,{aw:s(cu,t,function(){return""},n.aw)})}),su=function(r){return b($e,vu(r),au)},bu=t(function(r,n,t){var e=nt(r.Z);return b(It,b(su,r.k,n),{V:t,k:r.k,u:pn,S:e})}),du=function(r){for(;;){r=r}},lu=ur,hu=lu(0),$u=ir,gu=e(function(n,r){return b($u,function(r){return lu(n(r))},r)}),pu=t(function(t,r,e){return b($u,function(n){return b($u,function(r){return lu(b(t,n,r))},e)},r)}),mu=pr,yu=e(function(r,n){var t=n;return cr(b($u,mu(r),t))});$r.Task={b:hu,c:t(function(r,n){return b(gu,function(){return 0},(t=b(me,yu(r),n),s(pe,pu(kn),lu(h),t)));var t}),d:t(function(){return lu(0)}),e:e(function(r,n){return b(gu,r,n)}),f:gn};var wu,Cu=mr("Task"),Au=e(function(r,n){return Cu(b(gu,r,n))}),ku=function(r){return{$:0,a:r}},ju=function(r){switch(r.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Lu=e(function(r,n){return r<1?n:s(Yt,r,ue(n),n)}),Nu=Z,_u=e(function(r,n){return r<1?"":s(Yt,0,r,n)}),xu=D,Eu=function(r){for(var n=0,t=r.charCodeAt(0),e=43==t||45==t?1:0,u=e;u<r.length;++u){var a=r.charCodeAt(u);if(a<48||57<a)return Wn;n=10*n+a-48}return u==e?Wn:Un(45==t?-n:n)},Ou=i(function(r,n,t,e,u,a){return{al:a,an:n,aw:e,ay:t,aC:r,aD:u}}),Tu=a(function(r,n,t,e,u){if(ee(u)||b(xu,"@",u))return Wn;var a=b(Nu,":",u);if(a.b){if(a.b.b)return Wn;var i=a.a,o=Eu(b(Lu,i+1,u));if(1===o.$)return Wn;var f=o;return Un(c(Ou,r,b(_u,i,u),f,n,t,e))}return Un(c(Ou,r,u,Wn,n,t,e))}),Iu=u(function(r,n,t,e){if(ee(e))return Wn;var u=b(Nu,"/",e);if(u.b){var a=u.a;return l(Tu,r,b(Lu,a,e),n,t,b(_u,a,e))}return l(Tu,r,"/",n,t,e)}),Su=t(function(r,n,t){if(ee(t))return Wn;var e=b(Nu,"?",t);if(e.b){var u=e.a;return d(Iu,r,Un(b(Lu,u+1,t)),n,b(_u,u,t))}return d(Iu,r,Wn,n,t)}),zu=e(function(r,n){if(ee(n))return Wn;var t=b(Nu,"#",n);if(t.b){var e=t.a;return s(Su,r,Un(b(Lu,e+1,n)),b(_u,e,n))}return s(Su,r,Wn,n)}),Ru=function(r){return b(ae,"http://",r)?b(zu,0,b(Lu,7,r)):b(ae,"https://",r)?b(zu,1,b(Lu,8,r)):Wn},Fu=function(r){return b(Au,du,ar(function(){try{ln.location=r}catch(r){Nr.location.reload(!1)}}))},Gu=bn,Hu=e(function(r,n){return 1===r.$?n:n+":"+xt(r.a)}),Ku=t(function(r,n,t){return 1===n.$?t:_(t,_(r,n.a))}),Mu=e(function(r,n){if(r.$)return b(It,b(su,n.k,r.a),n);var t=r.a;return k(n,t.$?Fu(t.a):b(Gu,n.V,function(r){return s(Ku,"#",r.al,s(Ku,"?",r.aD,_(b(Hu,r.ay,_(r.aC?"https://":"http://",r.an)),r.aw)))}(t.a)))}),qu=Or("a"),Du=e(function(r,n){return b(Sr,function(r){return/^(on|formAction$)/i.test(r)?"data-"+r:r}(r),function(r){return/^\s*(javascript:|data:text\/html)/i.test(r)?"":r}(n))}),Bu=tr,Pu=e(function(r,n){return b(Ir,r,Bu(n))}),Zu=Pu("className"),Vu=function(r){return b(Pu,"href",/^javascript:/i.test((n=r).replace(/\s/g,""))?"":n);var n},Yu=Er("http://www.w3.org/2000/svg"),Ju=Yu("path"),Uu=Yu("svg"),Wu=Sr("class"),Qu=Sr("d"),Xu=Sr("fill"),ra=Sr("height"),na=Sr("style"),ta=Sr("viewBox"),ea=Sr("width"),ua=b(qu,g([Vu("https://github.com/ravicious/random-color"),Zu("github-corner"),b(Du,"aria-label","View source on Github")]),g([b(Uu,g([ea("80"),ra("80"),ta("0 0 250 250"),na("fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;")]),g([b(Ju,g([Qu("M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z")]),h),b(Ju,g([Qu("M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2"),Xu("currentColor"),na("transform-origin: 130px 106px;"),Wu("octo-arm")]),h),b(Ju,g([Qu("M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z"),Xu("currentColor"),Wu("octo-body")]),h)]))])),aa=t(function(r,n,t){return 0<r?s(aa,r>>1,_(n,n),1&r?_(t,n):t):t}),ia=e(function(r,n){return s(aa,r,n,"")}),oa=t(function(r,n,t){return _(b(ia,r-ue(t),ce(n)),t)}),fa=function(r){return p(r).join("")},ca=function(r){r:for(;;)switch(r){case 0:return"0";case 1:return"1";case 2:return"2";case 3:return"3";case 4:return"4";case 5:return"5";case 6:return"6";case 7:return"7";case 8:return"8";case 9:return"9";case 10:return"a";case 11:return"b";case 12:return"c";case 13:return"d";case 14:return"e";case 15:return"f";default:r=r;continue r}},va=e(function(r,n){for(;;){if(n<16)return b(kn,ca(n),r);r=b(kn,ca(b(_n,16,n)),r),n=n/16|0}}),sa=function(r){return fa(r<0?b(kn,"-",b(va,h,-r)):b(va,h,r))},ba=function(r){var n,t=b($e,sa,b(oa,2,"0")),e=Gn(r),u=e.I,a=e.G;return n=g([t(e.K),t(u),t(a)]),b(Et,"",n)},da=function(r){var n=Gn(r),t=n.I,e=n.G;return"rgb("+xt(n.K)+", "+xt(t)+", "+xt(e)+")"},la=Or("div"),ha=Tr,$a=e(function(r,n){return b(la,b(Ie,g([b(ha,"height","100vh"),b(ha,"width","100vw"),b(ha,"display","flex"),b(ha,"flex-direction","column"),b(ha,"justify-content","center"),b(ha,"align-items","center"),b(ha,"font-family","monospace"),b(ha,"font-size","calc(.75rem + 5.5vw)"),b(ha,"text-align","center")]),r),n)}),ga=Or("br"),pa=xr,ma=P,ya=function(r){return Or(function(r){return"script"==r?"p":r}(r))},wa=function(n){var r=n.a4,i=n.a5,t=function(){t.a(r(sn()))};return fn({L:function(a){return t.a=a,ln.addEventListener("popstate",t),ln.navigator.userAgent.indexOf("Trident")<0||ln.addEventListener("hashchange",t),e(function(r,n){if(!(n.ctrlKey||n.metaKey||n.shiftKey||1<=n.button||r.target||r.download)){n.preventDefault();var t=r.href,e=sn(),u=Ru(t).a;a(i(u&&e.aC===u.aC&&e.an===u.an&&e.ay.a===u.ay.a?{$:0,a:u}:function(r){return{$:1,a:r}}(t)))}})},a$:function(r){return s(n.a$,r,sn(),t)},bd:n.bd,bb:n.bb,a9:n.a9})},Ca=yr(h),Aa=Y,ka=V,ja={$:2},La={$:6},Na=wa({a$:bu,a4:function(r){return{$:1,a:r}},a5:function(r){return{$:0,a:r}},a9:Vt(Ca),bb:Mu,bd:function(r){return{aS:g([s(ya,"base",g([Vu(b(ma,"/",r.k)?r.k:r.k+"/")]),h),ua,function(r){var n=r.u;switch(n.$){case 2:var t=n.a.F,e=da(n.a.N),u=da(t);return b($a,g([b(ha,"background-color",u),b(ha,"color",e)]),g([pa(da(t)),b(ga,h,h),pa("#"+ba(t)),b(la,g([b(ha,"font-size","calc(.55rem + 1.0vw)"),b(ha,"line-height","1.5em")]),g([b(qu,g([b(ha,"color",e),Vu("colors/"+ba(t)),b(ha,"display","block")]),g([pa("link to this color")])),b(qu,g([b(ha,"color",e),Vu("")]),g([pa("generate random color")]))]))]));case 1:return t=n.a.F,e=da(n.a.N),u=da(t),b($a,g([b(ha,"background-color",u),b(ha,"color",e)]),g([pa(da(t)),b(ga,h,h),pa("#"+ba(t)),b(qu,g([b(ha,"font-size","calc(.55rem + 1.0vw)"),b(ha,"color",e),Vu("")]),g([pa("generate random color")]))]));default:return b($a,g([b(ha,"background-color","white"),b(ha,"color","black")]),g([pa("Incorrect color"),b(qu,g([b(ha,"font-size","calc(.55rem + 1.0vw)"),Vu("")]),g([pa("generate random color")]))]))}}(r)]),ba:"Random Color"}}});wu={Main:{init:Na(b(Aa,function(n){return b(Aa,function(r){return ku({k:r,Z:n})},b(ka,"mountPath",La))},b(ka,"randomNumber",ja)))(0)}},r.Elm?function r(n,t){for(var e in t)e in n?"init"==e?O(6):r(n[e],t[e]):n[e]=t[e]}(r.Elm,wu):r.Elm=wu}(this);