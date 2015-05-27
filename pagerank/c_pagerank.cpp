#include <cstdio>
#include <vector>
#include <algorithm>

using std::vector;

struct pagerank_t {
  const int n;
  const double eps, damping;
  vector<int> sinks, ed;
  vector<double> invOutWeight;
  int *gT;
};

void next_rank(const pagerank_t &page, const vector<double> &p, vector<double> &p_nxt) {
  double ins_z = 0;
  for (int v : page.sinks)
    ins_z += p[v];
  const double baseVal = 1 - page.damping + page.damping/page.n * ins_z;
  for (double &p : p_nxt)
    p = baseVal;
  for (int u = 0, *pv = page.gT; u != page.n; ++u) {
    for (; pv != page.gT + page.ed[u]; ++pv)
      p_nxt[u] += p[*pv]*page.invOutWeight[*pv];
  }
}

double l2norm2(const pagerank_t& page, const vector<double> &p1, const vector<double> &p2) {
  double diff = 0;
  for (auto cit = p1.cbegin(), cjt = p2.cbegin(); diff <= page.eps && cit != p1.cend(); ++cit, ++cjt)
    diff += (*cit - *cjt)*(*cit - *cjt);
  return diff;
}

vector<double> iter_pagerank(const pagerank_t &page) {
  vector<double> p(page.n, 1.0), p_nxt(page.n);
  do {
    next_rank(page, p, p_nxt);
    p.swap(p_nxt);
    fprintf(stderr, ".");
  } while (l2norm2(page, p, p_nxt) > page.eps);
  return std::move(p);
}

int gn() {
  int ch = getchar();
  while (ch!=EOF && (ch<'0' || '9'<ch)) ch = getchar();
  if (ch == EOF) return -1;
  int val = 0;
  while ('0'<=ch && ch<='9') {
    val = val*10 + ch - '0';
    ch = getchar();
  }
  return val;
}

int main() {
  int n, u, d;

  scanf("%*s%d", &n);
  pagerank_t page { n, 1e-6, 0.85 };

  fprintf(stderr, "n = %d\n", page.n);
  page.ed.resize(page.n);
  page.invOutWeight.resize(page.n);
  vector<vector<int>> gT(page.n);

  fprintf(stderr, "Reading graph...\n");
  while ((u = gn()) >= 0) {
    d = gn();
    page.invOutWeight[u-1] = static_cast<double>(page.damping)/d;
    while (d--) {
      int v = gn();
      gT[v-1].push_back(u-1);
      ++page.ed[v-1];
    }
  }

  fprintf(stderr, "Building graph...\n");
  for (auto it = ++page.ed.begin(); it != page.ed.end(); ++it)
    *it += *(it-1);
  page.gT = new int[page.ed.back()];

  for (int u = 0, *pv = page.gT; u != page.n; ++u) {
    for (int v : gT[u])
      *pv++ = v;
  }

  fprintf(stderr, "Sink nodes...\n");
  for (int u = 0; u != page.n; ++u)
    if (page.invOutWeight[u] == 0)
      page.sinks.push_back(u);

  fprintf(stderr, "Calculating PageRank");
  vector<double> p {iter_pagerank(page)};
  delete[] page.gT;

  fprintf(stderr, "\nOutputing...\n");

  for (int i = 0; i < page.n; ++i)
    printf("%d:%.10f\n", i+1, p[i]);
  return 0;
}