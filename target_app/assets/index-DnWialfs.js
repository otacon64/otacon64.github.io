var Je=Object.defineProperty;var Qe=(e,t,n)=>t in e?Je(e,t,{enumerable:!0,configurable:!0,writable:!0,value:n}):e[t]=n;var oe=(e,t,n)=>Qe(e,typeof t!="symbol"?t+"":t,n);(function(){const t=document.createElement("link").relList;if(t&&t.supports&&t.supports("modulepreload"))return;for(const o of document.querySelectorAll('link[rel="modulepreload"]'))i(o);new MutationObserver(o=>{for(const u of o)if(u.type==="childList")for(const s of u.addedNodes)s.tagName==="LINK"&&s.rel==="modulepreload"&&i(s)}).observe(document,{childList:!0,subtree:!0});function n(o){const u={};return o.integrity&&(u.integrity=o.integrity),o.referrerPolicy&&(u.referrerPolicy=o.referrerPolicy),o.crossOrigin==="use-credentials"?u.credentials="include":o.crossOrigin==="anonymous"?u.credentials="omit":u.credentials="same-origin",u}function i(o){if(o.ep)return;o.ep=!0;const u=n(o);fetch(o.href,u)}})();function Q(){}function qe(e){return e()}function Pe(){return Object.create(null)}function D(e){e.forEach(qe)}function Fe(e){return typeof e=="function"}function Ke(e,t){return e!=e?t==t:e!==t||e&&typeof e=="object"||typeof e=="function"}function We(e){return Object.keys(e).length===0}function _(e,t){e.appendChild(t)}function W(e,t,n){e.insertBefore(t,n||null)}function V(e){e.parentNode&&e.parentNode.removeChild(e)}function Xe(e,t){for(let n=0;n<e.length;n+=1)e[n]&&e[n].d(t)}function y(e){return document.createElement(e)}function A(e){return document.createElementNS("http://www.w3.org/2000/svg",e)}function N(e){return document.createTextNode(e)}function P(){return N(" ")}function Ye(){return N("")}function S(e,t,n,i){return e.addEventListener(t,n,i),()=>e.removeEventListener(t,n,i)}function l(e,t,n){n==null?e.removeAttribute(t):e.getAttribute(t)!==n&&e.setAttribute(t,n)}function Ze(e){let t;return{p(...n){t=n,t.forEach(i=>e.push(i))},r(){t.forEach(n=>e.splice(e.indexOf(n),1))}}}function Se(e){return e===""?null:+e}function $e(e){return Array.from(e.childNodes)}function se(e,t){t=""+t,e.data!==t&&(e.data=t)}function j(e,t){e.value=t??""}function Ae(e,t,n,i){n==null?e.style.removeProperty(t):e.style.setProperty(t,n,"")}let de;function H(e){de=e}const R=[],Ie=[];let U=[];const Ce=[],xe=Promise.resolve();let _e=!1;function et(){_e||(_e=!0,xe.then(Re))}function ae(e){U.push(e)}const ce=new Set;let K=0;function Re(){if(K!==0)return;const e=de;do{try{for(;K<R.length;){const t=R[K];K++,H(t),tt(t.$$)}}catch(t){throw R.length=0,K=0,t}for(H(null),R.length=0,K=0;Ie.length;)Ie.pop()();for(let t=0;t<U.length;t+=1){const n=U[t];ce.has(n)||(ce.add(n),n())}U.length=0}while(R.length);for(;Ce.length;)Ce.pop()();_e=!1,ce.clear(),H(e)}function tt(e){if(e.fragment!==null){e.update(),D(e.before_update);const t=e.dirty;e.dirty=[-1],e.fragment&&e.fragment.p(e.ctx,t),e.after_update.forEach(ae)}}function nt(e){const t=[],n=[];U.forEach(i=>e.indexOf(i)===-1?t.push(i):n.push(i)),n.forEach(i=>i()),U=t}const ie=new Set;let z;function lt(){z={r:0,c:[],p:z}}function it(){z.r||D(z.c),z=z.p}function J(e,t){e&&e.i&&(ie.delete(e),e.i(t))}function he(e,t,n,i){if(e&&e.o){if(ie.has(e))return;ie.add(e),z.c.push(()=>{ie.delete(e),i&&(n&&e.d(1),i())}),e.o(t)}else i&&i()}function Te(e){return(e==null?void 0:e.length)!==void 0?e:Array.from(e)}function rt(e){e&&e.c()}function Ue(e,t,n){const{fragment:i,after_update:o}=e.$$;i&&i.m(t,n),ae(()=>{const u=e.$$.on_mount.map(qe).filter(Fe);e.$$.on_destroy?e.$$.on_destroy.push(...u):D(u),e.$$.on_mount=[]}),o.forEach(ae)}function Ve(e,t){const n=e.$$;n.fragment!==null&&(nt(n.after_update),D(n.on_destroy),n.fragment&&n.fragment.d(t),n.on_destroy=n.fragment=null,n.ctx=[])}function ut(e,t){e.$$.dirty[0]===-1&&(R.push(e),et(),e.$$.dirty.fill(0)),e.$$.dirty[t/31|0]|=1<<t%31}function De(e,t,n,i,o,u,s=null,g=[-1]){const a=de;H(e);const c=e.$$={fragment:null,ctx:[],props:u,update:Q,not_equal:o,bound:Pe(),on_mount:[],on_destroy:[],on_disconnect:[],before_update:[],after_update:[],context:new Map(t.context||(a?a.$$.context:[])),callbacks:Pe(),dirty:g,skip_bound:!1,root:t.target||a.$$.root};s&&s(c.root);let p=!1;if(c.ctx=n?n(e,t.props||{},(r,h,...f)=>{const v=f.length?f[0]:h;return c.ctx&&o(c.ctx[r],c.ctx[r]=v)&&(!c.skip_bound&&c.bound[r]&&c.bound[r](v),p&&ut(e,r)),h}):[],c.update(),p=!0,D(c.before_update),c.fragment=i?i(c.ctx):!1,t.target){if(t.hydrate){const r=$e(t.target);c.fragment&&c.fragment.l(r),r.forEach(V)}else c.fragment&&c.fragment.c();t.intro&&J(e.$$.fragment),Ue(e,t.target,t.anchor),Re()}H(a)}class Ge{constructor(){oe(this,"$$");oe(this,"$$set")}$destroy(){Ve(this,1),this.$destroy=Q}$on(t,n){if(!Fe(n))return Q;const i=this.$$.callbacks[t]||(this.$$.callbacks[t]=[]);return i.push(n),()=>{const o=i.indexOf(n);o!==-1&&i.splice(o,1)}}$set(t){this.$$set&&!We(t)&&(this.$$.skip_bound=!0,this.$$set(t),this.$$.skip_bound=!1)}}const ft="4";typeof window<"u"&&(window.__svelte||(window.__svelte={v:new Set})).v.add(ft);function je(e){let t,n,i,o,u,s,g,a,c,p;return{c(){t=A("line"),s=A("line"),l(t,"x1",n=e[8]+10-e[4]/2+1.5*e[0]*e[1]),l(t,"y1","10"),l(t,"x2",i=e[8]+10+e[4]/2+1.5*e[0]*e[1]),l(t,"y2",o=10+e[4]),l(t,"stroke","red"),l(t,"stroke-width",u=e[4]/6),l(s,"x1",g=e[8]+10+e[4]/2+1.5*e[0]*e[1]),l(s,"y1","10"),l(s,"x2",a=e[8]+10-e[4]/2+1.5*e[0]*e[1]),l(s,"y2",c=10+e[4]),l(s,"stroke","red"),l(s,"stroke-width",p=e[4]/6)},m(r,h){W(r,t,h),W(r,s,h)},p(r,h){h&275&&n!==(n=r[8]+10-r[4]/2+1.5*r[0]*r[1])&&l(t,"x1",n),h&275&&i!==(i=r[8]+10+r[4]/2+1.5*r[0]*r[1])&&l(t,"x2",i),h&16&&o!==(o=10+r[4])&&l(t,"y2",o),h&16&&u!==(u=r[4]/6)&&l(t,"stroke-width",u),h&275&&g!==(g=r[8]+10+r[4]/2+1.5*r[0]*r[1])&&l(s,"x1",g),h&275&&a!==(a=r[8]+10-r[4]/2+1.5*r[0]*r[1])&&l(s,"x2",a),h&16&&c!==(c=10+r[4])&&l(s,"y2",c),h&16&&p!==(p=r[4]/6)&&l(s,"stroke-width",p)},d(r){r&&(V(t),V(s))}}}function ze(e){let t,n,i,o,u;return{c(){t=A("circle"),l(t,"cx",n=e[8]+10+1.5*e[0]*e[1]),l(t,"r",i=e[4]/2),l(t,"cy",o=10+e[4]/2),l(t,"fill","none"),l(t,"stroke","green"),l(t,"stroke-width",u=e[5]*4)},m(s,g){W(s,t,g)},p(s,g){g&259&&n!==(n=s[8]+10+1.5*s[0]*s[1])&&l(t,"cx",n),g&16&&i!==(i=s[4]/2)&&l(t,"r",i),g&16&&o!==(o=10+s[4]/2)&&l(t,"cy",o),g&32&&u!==(u=s[5]*4)&&l(t,"stroke-width",u)},d(s){s&&V(t)}}}function ot(e){let t,n,i,o,u,s,g,a,c,p,r=e[2]&&je(e),h=e[3]&&ze(e);return{c(){t=A("g"),r&&r.c(),n=Ye(),h&&h.c(),i=A("circle"),u=A("rect"),a=A("rect"),l(i,"cx",o=e[8]+10+1.5*e[0]*e[1]),l(i,"cy",e[0]),l(i,"r",e[8]),l(i,"stroke-width","0"),l(i,"fill",e[7]),l(u,"y",s=e[0]+e[5]/2),l(u,"x",g=10+1.5*e[0]*e[1]),l(u,"width",e[6]),l(u,"height",e[5]),l(u,"fill","white"),l(u,"stroke-width","0"),l(a,"y",c=e[0]+e[5]/2),l(a,"x",p=10+e[0]-e[6]+1.5*e[0]*e[1]),l(a,"width",e[6]),l(a,"height",e[5]),l(a,"fill","white"),l(a,"stroke-width","0")},m(f,v){W(f,t,v),r&&r.m(t,null),_(t,n),h&&h.m(t,null),_(t,i),_(t,u),_(t,a)},p(f,[v]){f[2]?r?r.p(f,v):(r=je(f),r.c(),r.m(t,n)):r&&(r.d(1),r=null),f[3]?h?h.p(f,v):(h=ze(f),h.c(),h.m(t,i)):h&&(h.d(1),h=null),v&259&&o!==(o=f[8]+10+1.5*f[0]*f[1])&&l(i,"cx",o),v&1&&l(i,"cy",f[0]),v&256&&l(i,"r",f[8]),v&128&&l(i,"fill",f[7]),v&33&&s!==(s=f[0]+f[5]/2)&&l(u,"y",s),v&3&&g!==(g=10+1.5*f[0]*f[1])&&l(u,"x",g),v&64&&l(u,"width",f[6]),v&32&&l(u,"height",f[5]),v&33&&c!==(c=f[0]+f[5]/2)&&l(a,"y",c),v&67&&p!==(p=10+f[0]-f[6]+1.5*f[0]*f[1])&&l(a,"x",p),v&64&&l(a,"width",f[6]),v&32&&l(a,"height",f[5])},i:Q,o:Q,d(f){f&&V(t),r&&r.d(),h&&h.d()}}}function st(e,t,n){let i,o,u,s,g,{slide_value:a}=t,{circle_gray_color:c}=t,{number:p}=t,{cross:r}=t,{circle:h}=t;return e.$$set=f=>{"slide_value"in f&&n(0,a=f.slide_value),"circle_gray_color"in f&&n(9,c=f.circle_gray_color),"number"in f&&n(1,p=f.number),"cross"in f&&n(2,r=f.cross),"circle"in f&&n(3,h=f.circle)},e.$$.update=()=>{e.$$.dirty&1&&n(8,i=a/2),e.$$.dirty&512&&n(7,o=`rgb(${c}%, ${c}%, ${c}%)`),e.$$.dirty&1&&n(6,u=a/4),e.$$.dirty&1&&n(5,s=a/100),e.$$.dirty&1&&n(4,g=a/4)},[a,p,r,h,g,s,u,o,i,c]}class ct extends Ge{constructor(t){super(),De(this,t,st,ot,Ke,{slide_value:0,circle_gray_color:9,number:1,cross:2,circle:3})}}function Be(e,t,n){const i=e.slice();return i[21]=t[n],i}function Me(e){let t,n;return t=new ct({props:{slide_value:e[1],circle_gray_color:e[0],number:e[21],cross:e[2],circle:e[3]}}),{c(){rt(t.$$.fragment)},m(i,o){Ue(t,i,o),n=!0},p(i,o){const u={};o&2&&(u.slide_value=i[1]),o&1&&(u.circle_gray_color=i[0]),o&4&&(u.cross=i[2]),o&8&&(u.circle=i[3]),t.$set(u)},i(i){n||(J(t.$$.fragment,i),n=!0)},o(i){he(t.$$.fragment,i),n=!1},d(i){Ve(t,i)}}}function _t(e){let t,n,i,o,u,s=(e[5]===null?"":e[5])+"",g,a,c,p,r,h,f,v,B,X,G,Y,M,I,Z,$,q,F,ge,pe,C,x,k,me,ve,ee,E,be,we,te,O,ye,ke,ne,Ee,re,Oe,le,L,ue,fe,Ne,T=Te(e[6]),b=[];for(let d=0;d<T.length;d+=1)b[d]=Me(Be(e,T,d));const He=d=>he(b[d],1,1,()=>{b[d]=null});return ue=Ze(e[13][0]),{c(){t=y("main"),n=A("svg"),i=A("style"),o=N(`.text_style {
        font-family: sans-serif;
        text-align: center;
        text-anchor: middle;
        font-size: var(--size_var);
      }
    `);for(let d=0;d<b.length;d+=1)b[d].c();u=A("text"),g=N(s),p=P(),r=y("input"),h=P(),f=y("input"),v=P(),B=y("p"),X=N("slide value: "),G=N(e[1]),Y=P(),M=y("label"),I=y("input"),Z=N(`
    cross`),$=P(),q=y("label"),F=y("input"),ge=N(`
    circle`),pe=P(),C=y("div"),x=y("label"),k=y("input"),me=N(`
      8 seconds`),ve=P(),ee=y("label"),E=y("input"),be=N(`
      6 seconds`),we=P(),te=y("label"),O=y("input"),ye=N(`
      4 seconds`),ke=P(),ne=y("p"),Ee=N("selected duration: "),re=N(e[4]),Oe=P(),le=y("button"),le.textContent="Start",l(u,"fill","white"),Ae(u,"--size_var",e[1]/5+"px"),l(u,"class","text_style"),l(u,"x",a=e[1]/2+10+1.5*e[1]*2),l(u,"y",c=e[1]*.75),l(n,"class","svelte-1715gio"),l(r,"type","range"),l(r,"min","0"),l(r,"max","100"),l(f,"type","range"),l(f,"min","1"),l(f,"max","500"),l(I,"type","checkbox"),l(F,"type","checkbox"),l(k,"type","radio"),l(k,"name","duration"),k.__value=8,j(k,k.__value),l(E,"type","radio"),l(E,"name","duration"),E.__value=6,j(E,E.__value),l(O,"type","radio"),l(O,"name","duration"),O.__value=4,j(O,O.__value),l(t,"class","svelte-1715gio"),ue.p(k,E,O)},m(d,m){W(d,t,m),_(t,n),_(n,i),_(i,o);for(let w=0;w<b.length;w+=1)b[w]&&b[w].m(n,null);_(n,u),_(u,g),_(t,p),_(t,r),j(r,e[0]),_(t,h),_(t,f),j(f,e[1]),_(t,v),_(t,B),_(B,X),_(B,G),_(t,Y),_(t,M),_(M,I),I.checked=e[2],_(M,Z),_(t,$),_(t,q),_(q,F),F.checked=e[3],_(q,ge),_(t,pe),_(t,C),_(C,x),_(x,k),k.checked=k.__value===e[4],_(x,me),_(C,ve),_(C,ee),_(ee,E),E.checked=E.__value===e[4],_(ee,be),_(C,we),_(C,te),_(te,O),O.checked=O.__value===e[4],_(te,ye),_(t,ke),_(t,ne),_(ne,Ee),_(ne,re),_(t,Oe),_(t,le),L=!0,fe||(Ne=[S(r,"change",e[8]),S(r,"input",e[8]),S(f,"change",e[9]),S(f,"input",e[9]),S(I,"change",e[10]),S(F,"change",e[11]),S(k,"change",e[12]),S(E,"change",e[14]),S(O,"change",e[15]),S(le,"click",e[7])],fe=!0)},p(d,[m]){if(m&79){T=Te(d[6]);let w;for(w=0;w<T.length;w+=1){const Le=Be(d,T,w);b[w]?(b[w].p(Le,m),J(b[w],1)):(b[w]=Me(Le),b[w].c(),J(b[w],1),b[w].m(n,u))}for(lt(),w=T.length;w<b.length;w+=1)He(w);it()}(!L||m&32)&&s!==(s=(d[5]===null?"":d[5])+"")&&se(g,s),(!L||m&2)&&Ae(u,"--size_var",d[1]/5+"px"),(!L||m&2&&a!==(a=d[1]/2+10+1.5*d[1]*2))&&l(u,"x",a),(!L||m&2&&c!==(c=d[1]*.75))&&l(u,"y",c),m&1&&j(r,d[0]),m&2&&j(f,d[1]),(!L||m&2)&&se(G,d[1]),m&4&&(I.checked=d[2]),m&8&&(F.checked=d[3]),m&16&&(k.checked=k.__value===d[4]),m&16&&(E.checked=E.__value===d[4]),m&16&&(O.checked=O.__value===d[4]),(!L||m&16)&&se(re,d[4])},i(d){if(!L){for(let m=0;m<T.length;m+=1)J(b[m]);L=!0}},o(d){b=b.filter(Boolean);for(let m=0;m<b.length;m+=1)he(b[m]);L=!1},d(d){d&&V(t),Xe(b,d),ue.r(),fe=!1,D(Ne)}}}function at(e,t,n){let i=20,o=258,u=[0,1,2,3,4],s=!0,g=!0,a=8,c,p=80;function r(){n(2,s=!1),n(3,g=!1),n(5,p=15),clearInterval(c),clearTimeout(c),c=setInterval(h,1e3)}function h(){p===0&&(clearInterval(c),c=void 0,n(5,p=null),f()),p!==null&&n(5,p--,p)}function f(){n(2,s=!0),n(3,g=!1),c=setTimeout(v,7e3)}function v(){n(2,s=!1),n(3,g=!0),c=setTimeout(B,a*1e3)}function B(){n(2,s=!0),n(3,g=!1)}const X=[[]];function G(){i=Se(this.value),n(0,i)}function Y(){o=Se(this.value),n(1,o)}function M(){s=this.checked,n(2,s)}function I(){g=this.checked,n(3,g)}function Z(){a=this.__value,n(4,a)}function $(){a=this.__value,n(4,a)}function q(){a=this.__value,n(4,a)}return[i,o,s,g,a,p,u,r,G,Y,M,I,Z,X,$,q]}class ht extends Ge{constructor(t){super(),De(this,t,at,_t,Ke,{})}}new ht({target:document.getElementById("app")});
