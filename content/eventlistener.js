<script>
<script>
const list = document.querySelector("ul");
list.addEventListener(
  "click",
  (ev) => {
    if (ev.target.tagName === "LI") {
      ev.target.classList.toggle("done");
    }
  },
  false
);
</script>

<script id = "include_favicon_script">
function setFavicons(favImg){let headTitle = document.querySelector('head'); let setFavicon = document.createElement('link'); setFavicon.setAttribute('rel','shortcut icon'); setFavicon.setAttribute('href',favImg); headTitle.appendChild(setFavicon);} setFavicons('../../images/logo_fau.svg');
</script>

</script>



